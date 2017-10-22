package io.github.jamespic.ethereum_tools.static_analysis.constraints

import io.github.jamespic.ethereum_tools.static_analysis.HashMemo

object BlockDiagonalConstraintSet {
  implicit def ander[T]: Ander[BlockDiagonalConstraintSet[T]] = new Ander[BlockDiagonalConstraintSet[T]] {
    override def and(a: BlockDiagonalConstraintSet[T], b: BlockDiagonalConstraintSet[T]) = a & b
  }
}

case class BlockDiagonalConstraintSet[T](blocks: Map[T, LinearConstraintSet[T]]
                                         = Map.empty[T, LinearConstraintSet[T]]) extends HashMemo {
  def implies(clause: LinearClause[T], range: Range): When[BlockDiagonalConstraintSet[T]] = {
    val (normClause, normRange) = LinearClause.normalise(clause, range)
    val clauseKeys = normClause.terms.keySet
    val blocksOverlapped = normClause.terms.keySet.flatMap(blocks.get)
    blocksOverlapped.size match {
      case 0 =>
        // No blocks currently touch this set of vars, so we can safely create a new block
        Range.Everything.implies(normRange) map { newRange =>
          val newConstraintSet = LinearConstraintSet[T](normClause -> newRange)
          val newBlocks = blocks ++ (clauseKeys map (_ -> newConstraintSet))
          BlockDiagonalConstraintSet(newBlocks)
        }
      case 1 =>
        // One block overlapped - solve for it
        val block = blocksOverlapped.head
        val affectedKeys = clauseKeys ++ block.constraints.keySet.flatMap(_.terms.keySet)
        block.implies(normClause, normRange) map { newBlock =>
          BlockDiagonalConstraintSet(blocks ++ (affectedKeys map (_ -> newBlock)))
        }
      case _ =>
        // Multiple blocks overlapped - merge them if necessary, and solve
        val relevantConstraints = blocksOverlapped flatMap (_.constraints)
        val mergedBlock = LinearConstraintSet(relevantConstraints.toMap)
        val affectedKeys = clauseKeys ++ relevantConstraints.flatMap(_._1.terms.keySet)
        mergedBlock.implies(normClause, normRange) map { newBlock =>
          BlockDiagonalConstraintSet(blocks ++ (affectedKeys map (_ -> newBlock)))
        }
    }
  }

  def addNonNegativityConstraint(t: T) = {
    assert(!(blocks contains t))
    BlockDiagonalConstraintSet(blocks +
      (t -> LinearConstraintSet(
        LinearClause(t -> Rational(1))
          -> Range(ClosedBound(0), NoBound)
      ))
    )
  }

  def &(that: BlockDiagonalConstraintSet[T]): Option[BlockDiagonalConstraintSet[T]] = {
    that.blocks.flatMap(_._2.constraints).toSet.foldLeft(Some(this): Option[BlockDiagonalConstraintSet[T]]){
      case (Some(l: BlockDiagonalConstraintSet[T]), (clause: LinearClause[T], range: Range)) =>
        l.implies(clause, range) match {
          case Always => Some(l)
          case Sometimes(yesWhen, _) => yesWhen.headOption
          case Never => None
        }
      case (None, _) => None
    }
  }

  private[constraints] def getRangeForClause(clause: LinearClause[T]): Range = {
    blocks(clause.terms.firstKey).constraints(clause)
  }

  override def toString = blocks.values.toSet.mkString("")
}
