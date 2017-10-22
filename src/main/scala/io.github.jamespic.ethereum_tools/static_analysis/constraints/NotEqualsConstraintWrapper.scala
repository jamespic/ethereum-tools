package io.github.jamespic.ethereum_tools.static_analysis.constraints

object NotEqualsConstraintWrapper {
  def ander[T] = new Ander[NotEqualsConstraintWrapper[T]] {
    override def and(a: NotEqualsConstraintWrapper[T], b: NotEqualsConstraintWrapper[T]) = a & b
  }
}

// FIXME: Consider cases like inequality constraints at closed range boundaries
case class NotEqualsConstraintWrapper[T](rangeConstraints: BlockDiagonalConstraintSet[T] = BlockDiagonalConstraintSet[T](),
                                         notEqualConstraints: Set[(LinearClause[T], Rational)]
                                         = Set.empty[(LinearClause[T], Rational)]) {
  def implies(clause: LinearClause[T], range: Range) = {
    range match {
      case Range(ClosedBound(x), ClosedBound(y)) if x == y =>
        val normClause = LinearClause.normalise(clause, x)
        if (notEqualConstraints contains normClause) Never
        else rangeConstraints.implies(clause, range) match {
          case Always => Always
          case Never => Never
          case Sometimes(whenYes, whenNo) =>
            Sometimes(
              whenYes map (newRangeConstraints =>
                NotEqualsConstraintWrapper(newRangeConstraints, notEqualConstraints)),
              Set(NotEqualsConstraintWrapper(rangeConstraints, notEqualConstraints + normClause))
            )
        }
      case _ =>
        rangeConstraints.implies(clause, range) map (newRangeConstraints =>
          NotEqualsConstraintWrapper(newRangeConstraints, notEqualConstraints))
    }
  }

  def impliesNot(clause: LinearClause[T], value: Rational): When[NotEqualsConstraintWrapper[T]] = {
    val normEntry = LinearClause.normalise(clause, value)
    val (normClause, normValue) = normEntry
    if (notEqualConstraints contains normEntry) Always
    else rangeConstraints.implies(normClause, Range.singlePoint(normValue)) match {
      case Always => Never
      case Never => Always
      case Sometimes(whenYes, _) =>
        Sometimes(
          Set(NotEqualsConstraintWrapper(rangeConstraints, notEqualConstraints + normEntry)),
          whenYes map (newRangeConstraints => NotEqualsConstraintWrapper(newRangeConstraints, notEqualConstraints))
        )
    }
  }

  def &(that: NotEqualsConstraintWrapper[T]) = for (rangeConstraint <- this.rangeConstraints & that.rangeConstraints) yield {
    NotEqualsConstraintWrapper(rangeConstraint, this.notEqualConstraints intersect that.notEqualConstraints)
  }

  override def toString = {
    rangeConstraints.toString + "\n" + notEqualConstraints.map {
      case (clause, value) if value.denom == 1 => s"$clause != 0x${value.num.toString(16)}"
      case (clause, value) => s"$clause != $value"
    }.mkString("\n")
  }

  def addNonNegativityConstraint(t: T) = copy(rangeConstraints = rangeConstraints.addNonNegativityConstraint(t))
}
