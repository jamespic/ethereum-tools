package io.github.jamespic.ethereum_tools.static_analysis.constraints

object Rational extends Fractional[Rational] {
  def apply(num: BigInt, denom: BigInt) = {
    val gcd = num.gcd(denom) * (if (denom < 0) -1 else 1)
    new Rational(num / gcd, denom / gcd)
  }
  implicit def apply(num: Int): Rational = Rational(num, 1)
  implicit def apply(num: BigInt): Rational = Rational(num, 1)
  def unapply(r: Rational) = Some((r.num, r.denom))

  implicit def numeric: Fractional[Rational] = this

  override def plus(x: Rational, y: Rational): Rational = x + y
  override def minus(x: Rational, y: Rational): Rational = x - y
  override def times(x: Rational, y: Rational): Rational = x * y
  override def div(x: Rational, y: Rational): Rational = x / y
  override def negate(x: Rational): Rational = -x
  override def fromInt(x: Int): Rational = Rational(x, 1)
  override def toInt(x: Rational): Int = x.toInt
  override def toLong(x: Rational): Long = x.toLong
  override def toFloat(x: Rational): Float = x.toFloat
  override def toDouble(x: Rational): Double = x.toDouble
  override def compare(x: Rational, y: Rational): Int = x compare y
}

class Rational private (val num: BigInt, val denom: BigInt) extends Ordered[Rational] {
  if (denom == 0) throw new ArithmeticException("Cannot divide by 0")
  def +(that: Rational) = Rational(this.num * that.denom + that.num * this.denom, this.denom * that.denom)
  def -(that: Rational) = Rational(this.num * that.denom - that.num * this.denom, this.denom * that.denom)
  def *(that: Rational) = Rational(this.num * that.num, this.denom * that.denom)
  def /(that: Rational) = Rational(this.num * that.denom, this.denom * that.num)
  def unary_- = Rational(-num, denom)
  def toBigInt = num / denom
  def toInt = toBigInt.toInt
  def toLong = toBigInt.toLong
  def toFloat = toBigInt.toFloat
  def toDouble = toBigInt.toDouble
  override def toString = if (denom == 1) num.toString else s"$num / $denom"
  override def hashCode = num.hashCode ^ denom.hashCode
  override def equals(that: Any): Boolean = that match {
    case Rational(n, d) => num == n && denom == d
    case x => num == x && denom == 1
  }
  override def compare(that: Rational) = {
    (this - that).num compare 0
  }
}


