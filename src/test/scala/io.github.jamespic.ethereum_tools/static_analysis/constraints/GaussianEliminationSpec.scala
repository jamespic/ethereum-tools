package io.github.jamespic.ethereum_tools.static_analysis.constraints

import org.scalatest.{FreeSpec, Matchers}

class GaussianEliminationSpec extends FreeSpec with Matchers {
  "GaussianElimination" - {
    "should build an upper echelon matrix, that can be used to express vectors in terms of other vectors" in {
      val state = LinearConstraintSet.GaussianEliminationState[V]()
      val Left(state1) = state.gaussianElimination(X + Y)
      val Left(state2) = state1.gaussianElimination(X * 2 - Z * 2)
      val Right(combo) = state2.gaussianElimination(Y + Z)
      val expected1 = LinearClause(
        (X * 2 - Z * 2) -> Rational(-1, 2),
        (X + Y) -> Rational(1))
      combo should equal(expected1)
      val Left(state3) = state2.gaussianElimination(Y)
      val Right(combo2) = state3.gaussianElimination(X + Y + Z)
      val expected2 = LinearClause(
        (X * 2 - Z * 2) -> Rational(-1, 2),
        (X + Y) -> Rational(2),
        (Y: LinearClause[V]) -> Rational(-1)
      )
      combo2 should equal(expected2)
    }
  }
}

