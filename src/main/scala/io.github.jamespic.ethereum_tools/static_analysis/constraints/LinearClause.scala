package io.github.jamespic.ethereum_tools.static_analysis.constraints

import io.github.jamespic.ethereum_tools.static_analysis.HashMemo

import scala.collection.SortedMap

object LinearClause {
  def apply[T](terms: Iterable[(T, Rational)]): LinearClause[T] = {
    var result = SortedMap.empty[T, Rational](ArbitraryOrdering[T])
    for ((term, factor) <- terms) {
      assert(factor.num != 0)
      result.get(term) match {
        case Some(x) =>
          val newValue = x + factor
          if (newValue.num == 0) result -= term
          else result += term -> newValue
        case None => result += term -> factor
      }
    }
    LinearClause(result)
  }
  def apply[T](terms: (T, Rational)*): LinearClause[T] = apply(terms)

  private[constraints] def normalise[T](clause: LinearClause[T], range: Range): (LinearClause[T], Range) = {
    clause.terms.headOption match {
      case Some((_, leadTerm)) => clause / leadTerm -> range / leadTerm
      case None => clause -> range
    }

  }

  private[constraints] def normalise[T](clause: LinearClause[T], value: Rational): (LinearClause[T], Rational) = {
    val leadTerm = clause.terms.head._2
    clause / leadTerm -> value / leadTerm
  }

  private[constraints] def normalise[T](clause: LinearClause[T]): LinearClause[T] = {
    val leadTerm = clause.terms.head._2
    clause / leadTerm
  }
}

case class LinearClause[T](terms: SortedMap[T, Rational]) extends HashMemo {
  def *(factor: Rational) = LinearClause(for ((k, v) <- terms) yield k -> v * factor)
  def /(factor: Rational) = LinearClause(for ((k, v) <- terms) yield k -> v / factor)
  def +(that: LinearClause[T]) = LinearClause(this.terms.toList ++ that.terms.toList)
  def -(that: LinearClause[T]) = this + (-that)
  def unary_- = LinearClause(terms.mapValues(-_))

  override def toString = (for ((v, factor) <- terms) yield {
    if (factor != Rational.One ) s"$factor * $v" else s"$v"
  }).mkString(" + ")
}


