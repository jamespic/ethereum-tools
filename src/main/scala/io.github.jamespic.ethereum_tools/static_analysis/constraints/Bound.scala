package io.github.jamespic.ethereum_tools.static_analysis.constraints

import io.github.jamespic.ethereum_tools.static_analysis.HashMemo

sealed trait Bound {
  def *(y: Rational): Bound
  def /(y: Rational): Bound
  def +(y: Rational): Bound
  def -(y: Rational): Bound
  def opposite: Bound
}
case object NoBound extends Bound {
  override def *(y: Rational): Bound = this
  override def /(y: Rational): Bound = this
  override def +(y: Rational): Bound = this
  override def -(y: Rational): Bound = this
  override def opposite = this // Probably shouln't be called
}
case class OpenBound(x: Rational) extends Bound with HashMemo {
  override def *(y: Rational): Bound = OpenBound(x * y)
  override def /(y: Rational): Bound = OpenBound(x / y)
  override def +(y: Rational): Bound = OpenBound(x + y)
  override def -(y: Rational): Bound = OpenBound(x - y)
  override def opposite = ClosedBound(x)
}
case class ClosedBound(x: Rational) extends Bound with HashMemo {
  override def *(y: Rational): Bound = ClosedBound(x * y)
  override def /(y: Rational): Bound = ClosedBound(x / y)
  override def +(y: Rational): Bound = ClosedBound(x + y)
  override def -(y: Rational): Bound = ClosedBound(x - y)
  override def opposite = OpenBound(x)
}
