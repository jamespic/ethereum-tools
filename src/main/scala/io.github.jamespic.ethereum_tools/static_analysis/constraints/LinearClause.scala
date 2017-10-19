package io.github.jamespic.ethereum_tools.static_analysis.constraints

import io.github.jamespic.ethereum_tools.static_analysis.HashMemo

import scala.collection.SortedMap
import scala.collection.mutable.{Map => MMap}

object LinearClause {
  def apply[T](terms: Iterable[(T, Rational)]): LinearClause[T] = {
    val sums = MMap.empty[T, Rational]
    for ((term, factor) <- terms) sums(term) = factor + sums.getOrElse(term, Rational(0))
    val zeroesFiltered = sums.toList.filter{case (k, Rational(n, d)) => n != BigInt(0)}
    LinearClause(SortedMap(zeroesFiltered: _*)(ArbitraryOrdering[T]))
  }
  def apply[T](terms: (T, Rational)*): LinearClause[T] = apply(terms)
}

case class LinearClause[T](terms: SortedMap[T, Rational]) extends HashMemo {
  def *(factor: Rational) = LinearClause(terms.mapValues(_ * factor))
  def /(factor: Rational) = LinearClause(terms.mapValues(_ / factor))
  def +(that: LinearClause[T]) = LinearClause(this.terms.toList ++ that.terms.toList)
  def -(that: LinearClause[T]) = this + (-that)
  def unary_- = LinearClause(terms.mapValues(-_))
}

