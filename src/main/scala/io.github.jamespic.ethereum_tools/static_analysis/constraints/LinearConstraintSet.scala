package io.github.jamespic.ethereum_tools.static_analysis.constraints

import io.github.jamespic.ethereum_tools.static_analysis.HashMemo

import scala.collection.SortedMap

object LinearConstraintSet {
  implicit def ander[T]: Ander[LinearConstraintSet[T]] = new Ander[LinearConstraintSet[T]] {
    override def and(a: LinearConstraintSet[T], b: LinearConstraintSet[T]) = a & b
  }

  def apply[T](constraints: (LinearClause[T], Range)*): LinearConstraintSet[T] = apply(constraints.toMap)

  private[constraints] type LinearCombination[T] = LinearClause[(LinearClause[T])]
  private[constraints] case class GaussianEliminationState[T](
      leadTermToRowMap: SortedMap[T, Row[T]] = SortedMap.empty[T, Row[T]](ArbitraryOrdering[T])) {
    def add(clause: LinearClause[T]) = {
      gaussianElimination(clause).left.toOption
    }
    def solveFor(clause: LinearClause[T]) = {
      gaussianElimination(clause).right.toOption
    }
    private[constraints] def gaussianElimination(clause: LinearClause[T]): Either[GaussianEliminationState[T], LinearCombination[T]] = {
      /*
       * To avoid duplicating code between adding rows and solving for rows, the gaussian elimination logic was factored
       * out into a separate method.
       *
       * It takes a linear clause, and either produces a new upper echelon linear combination that includes it,
       * or produces a linear combination of existing rows that adds up to it.
       */
      val oneRowLinearCombination = LinearClause(clause -> Rational.One)
      def eliminate(row: Row[T]): Either[GaussianEliminationState[T], LinearCombination[T]] = {
        row.clause.terms.headOption match {
          case Some((leadTerm, leadFactor)) =>
            // and check if there's already a row with this lead term
            leadTermToRowMap.get(leadTerm) match {
              case Some(existingRow) =>
                // There's already a row with this lead term, so we reduce by it, and try to insert the reduced row
                val nextRow = row - existingRow * leadFactor
                eliminate(nextRow)
              case None =>
                // We don't have a row with this lead term, fo we add it
                Left(
                  GaussianEliminationState(
                    leadTermToRowMap + (leadTerm -> row / leadFactor)
                  )
                )
            }
          case None =>
            // We've got no terms left, so we've made zero, so this row is a linear combination of the existing ones
            val decomposition = row.madeOfClauses
            val result = oneRowLinearCombination - decomposition
            // This assertion is too heavy duty to turn on in use, but it's helpful in testing
            //            assert(result.terms.map{
            //              case (clause, factor) => clause * factor
            //            }.reduce (_ + _) == clause)
            Right(result)
        }
      }
      eliminate(Row(clause, oneRowLinearCombination))
    }
  }

  private[constraints] case class Row[T](clause: LinearClause[T], madeOfClauses: LinearCombination[T]) {
    def +(that: Row[T]) = Row(this.clause + that.clause, this.madeOfClauses + that.madeOfClauses)
    def -(that: Row[T]) = Row(this.clause - that.clause, this.madeOfClauses - that.madeOfClauses)
    def *(x: Rational) = Row(this.clause * x, this.madeOfClauses * x)
    def /(x: Rational) = Row(this.clause / x, this.madeOfClauses / x)
  }
}

case class LinearConstraintSet[T](constraints: Map[LinearClause[T], Range]) extends HashMemo {
  import LinearConstraintSet._
  def implies(clause: LinearClause[T], range: Range): When[LinearConstraintSet[T]] = {
    if (clause.terms.isEmpty) {
      return Range(ClosedBound(0), ClosedBound(0)) implies range match {
        case Always => Always
        case Never => Never
        case _ => throw new AssertionError(s"Couldn't decide if ${range.toString(0)}")
      }
    }
    val (normClause, normRange) = normaliseClause(clause, range)
    // As a shortcut, check if there is already a constraint for this clause. If there is, and it's incompatible,
    // we can safely return never
    if (constraints.get(normClause).map(_ implies normRange).contains(Never)) return Never

    // Find all (if any) ways to express the clause of the new constraint
    // in terms of clauses of existing constraints.
    def findLinearlyDependentClauses(clauses: Iterable[LinearClause[T]]) = {
      def findRec(clauses: List[LinearClause[T]],
                  gaussianState: GaussianEliminationState[T]): Stream[LinearCombination[T]] = {
        clauses match {
          case head :: tail =>
            val newGaussianStateOption = gaussianState.add(head)
            val solutionOption = newGaussianStateOption.flatMap(_.solveFor(normClause))
            (newGaussianStateOption, solutionOption) match {
              case (Some(_), Some(solution)) => solution #:: findRec(tail, gaussianState)
              case (Some(newState), None) => findRec(tail, gaussianState) #::: findRec(tail, newState)
              case (None, _) => findRec(tail, gaussianState)
            }
          case Nil => Stream.empty
        }
      }
      findRec(clauses.toList, GaussianEliminationState())
    }
    val waysToMakeClause = findLinearlyDependentClauses(constraints.keys)
    // We use this to further restrict the range of values
    // our constraint can have.
    val rangeFromExistingConstraints = rangeIntersection(waysToMakeClause)
    assert(rangeFromExistingConstraints.nonEmpty)
    // Once we know the range allowed by the existing constraints, there are three possibilities:
    // 1) These range given by these existing constraints is a subset of the range our new constraint adds,
    //    so our new constraint is always satisfied
    // 2) The range given by these existing constraints overlaps with the range our new constraints adds,
    //    so our new constraint is satisfied within the intersection of these ranges, and not satisfied
    //    in the difference between these existing constraints and its new constraints
    // 3) The range given by these existing constraints is disjoint from the range of our new constraints,
    //    so our constraint is never satisfiable
    rangeFromExistingConstraints.get implies normRange map {range =>
      LinearConstraintSet(constraints + (normClause -> range))
    }
  }

  override def toString = (for ((clause, range) <- constraints) yield {
    range.toString(
      (for ((v, factor) <- clause.terms) yield if (factor != Rational.One ) s"$factor * $v" else s"$v").mkString(" + ")
    )
  }).mkString("\n", "\n", "\n")

  private def normaliseClause(clause: LinearClause[T], range: Range) = {
    val leadTerm = clause.terms.head._2
    clause / leadTerm -> range / leadTerm
  }

  private def rangeFromLinearCombination(combination: LinearCombination[T]): Range = {
    combination.terms.map{
      case (clause: LinearClause[T], factor: Rational) => constraints(clause) * factor
    }.reduce(_ + _)
  }

  private def rangeIntersection(combinations: Stream[LinearCombination[T]]): Option[Range] = {
    def rangeIntersectionRec(combinations: Stream[LinearCombination[T]], acc: Option[Range]): Option[Range] = {
      acc match {
        case Some(range) =>
          combinations match {
            case head #:: tail =>
              val rangeForThis = rangeFromLinearCombination(head)
              rangeIntersectionRec(tail, rangeForThis intersection range)
            case Stream.Empty => acc
          }
        case None => None
      }
    }
    rangeIntersectionRec(combinations, Some(Range.Everything))
  }

  def &(that: LinearConstraintSet[T]): Option[LinearConstraintSet[T]] = {
    that.constraints.foldLeft(Some(this): Option[LinearConstraintSet[T]]){
      case (Some(l: LinearConstraintSet[T]), (clause: LinearClause[T], range: Range)) =>
        l.implies(clause, range) match {
          case Always => Some(l)
          case Sometimes(yesWhen, _) => yesWhen.headOption
          case Never => None
        }
      case (None, _) => None
    }
  }
}


