package io.github.jamespic.ethereum_tools.static_analysis.constraints

object V {
  implicit def clause(x: V) = x!
}
sealed trait V {
  def *(i: Rational): LinearClause[V] = LinearClause(this -> i)
  def +(clause: LinearClause[V]): LinearClause[V] = V.clause(this) + clause
  def -(clause: LinearClause[V]): LinearClause[V] = V.clause(this) - clause
  def ! : LinearClause[V] = LinearClause(this -> Rational(1))
}
case object X extends V
case object Y extends V
case object Z extends V


