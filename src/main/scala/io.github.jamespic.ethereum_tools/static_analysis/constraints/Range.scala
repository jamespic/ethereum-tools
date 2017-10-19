package io.github.jamespic.ethereum_tools.static_analysis.constraints

import io.github.jamespic.ethereum_tools.static_analysis.HashMemo

case class Range(lowerBound: Bound, upperBound: Bound) extends HashMemo {
  assert(rangeSane)
  def *(y: Rational) = {
    if (y.num > 0) Range(lowerBound * y, upperBound * y)
    else Range(upperBound * y, lowerBound * y)
  }

  def /(y: Rational) = {
    if (y.num > 0) Range(lowerBound / y, upperBound / y)
    else Range(upperBound / y, lowerBound / y)
  }

  def +(y: Rational) = Range(lowerBound + y, upperBound + y)
  def -(y: Rational) = Range(lowerBound - y, upperBound - y)
  def +(that: Range) = {
    def addBounds(a: Bound, b: Bound) = (a, b) match {
      case (NoBound, _) | (_, NoBound) => NoBound
      case (OpenBound(x), OpenBound(y)) => OpenBound(x + y)
      case (ClosedBound(x), OpenBound(y)) => OpenBound(x + y)
      case (OpenBound(x), ClosedBound(y)) => OpenBound(x + y)
      case (ClosedBound(x), ClosedBound(y)) => ClosedBound(x + y)
    }
    Range(
      addBounds(this.lowerBound, that.lowerBound),
      addBounds(this.upperBound, that.upperBound)
    )
  }

  def intersection(that: Range): Option[Range] = {
    if (this disjoint that) None
    else Some(Range(
      if (otherHasHigherLowerBound(that)) that.lowerBound else this.lowerBound,
      if (otherHasLowerUpperBound(that)) that.upperBound else this.upperBound
    ))
  }

  def implies(that: Range): When[Range] = {
    val reducedUpperBound = otherHasLowerUpperBound(that)
    val increasedLowerBound = otherHasHigherLowerBound(that)
    if (disjoint(that)) Never
    else (reducedUpperBound, increasedLowerBound) match {
      case (false, false) => Always
      case (true, false) => Sometimes(
        Set(Range(this.lowerBound, that.upperBound)),
        Set(Range(that.upperBound.opposite, this.upperBound))
      )
      case (false, true) => Sometimes(
        Set(Range(that.lowerBound, this.upperBound)),
        Set(Range(this.lowerBound, that.lowerBound.opposite))
      )
      case (true, true) => Sometimes(
        Set(Range(that.lowerBound, that.upperBound)),
        Set(
          Range(that.upperBound.opposite, this.upperBound),
          Range(this.lowerBound, that.lowerBound.opposite)
        )
      )
    }
  }

  private def disjoint(that: Range) = {
    val impossibleLowerBound = (this.upperBound, that.lowerBound) match {
      case (NoBound, _) => false
      case (_, NoBound) => false
      case (OpenBound(x), OpenBound(y)) => x <= y
      case (ClosedBound(x), ClosedBound(y)) => x < y
      case (OpenBound(x), ClosedBound(y)) => x <= y
      case (ClosedBound(x), OpenBound(y)) => x <= y
    }
    val impossibleUpperBound = (this.lowerBound, that.upperBound) match {
      case (NoBound, _) => false
      case (_, NoBound) => false
      case (OpenBound(x), OpenBound(y)) => x >= y
      case (ClosedBound(x), ClosedBound(y)) => x > y
      case (OpenBound(x), ClosedBound(y)) => x >= y
      case (ClosedBound(x), OpenBound(y)) => x >= y
    }
    impossibleLowerBound || impossibleUpperBound
  }

  private def otherHasHigherLowerBound(that: Range) = {
    (this.lowerBound, that.lowerBound) match {
      case (_, NoBound) => false
      case (NoBound, _) => true
      case (OpenBound(x), OpenBound(y)) => y > x
      case (ClosedBound(x), ClosedBound(y)) => y > x
      case (OpenBound(x), ClosedBound(y)) => y > x
      case (ClosedBound(x), OpenBound(y)) => y >= x
    }
  }

  private def otherHasLowerUpperBound(that: Range) = {
    (this.upperBound, that.upperBound) match {
      case (_, NoBound) => false
      case (NoBound, _) => true
      case (OpenBound(x), OpenBound(y)) => y < x
      case (ClosedBound(x), ClosedBound(y)) => y < x
      case (OpenBound(x), ClosedBound(y)) => y < x
      case (ClosedBound(x), OpenBound(y)) => y <= x
    }
  }

  def rangeSane = (lowerBound, upperBound) match {
    case (NoBound, _) | (_, NoBound) => true
    case (OpenBound(x), OpenBound(y)) => x < y
    case (OpenBound(x), ClosedBound(y)) => x < y
    case (ClosedBound(x), OpenBound(y)) => x < y
    case (ClosedBound(x), ClosedBound(y)) => x <= y
  }

  override def toString = toString("x")
  def toString(varName: Any) = (lowerBound, upperBound) match {
    case (ClosedBound(x), ClosedBound(y)) if x == y => s"$varName == $x" // Format equality specially
    case _ =>
      val leftPart = lowerBound match {
        case NoBound => ""
        case OpenBound(x) => s"$x < "
        case ClosedBound(x) => s"$x <= "
      }
      val rightPart = upperBound match {
        case NoBound => ""
        case OpenBound(x) => s" < $x"
        case ClosedBound(x) => s" <= $x"
      }
      s"$leftPart$varName$rightPart"
  }
}