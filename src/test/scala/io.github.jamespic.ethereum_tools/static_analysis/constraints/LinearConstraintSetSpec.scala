package io.github.jamespic.ethereum_tools.static_analysis.constraints

import org.scalatest.{FreeSpec, Matchers}

class LinearConstraintSetSpec extends FreeSpec with Matchers {
  "LinearConstraintSet" - {
    "when working with a triangle" - {
      /*             (0, 1)
       *               +
       *              /  \
       *             /    \
       *            /      \
       * (-1/2, 0) +--------+ (1/2, 0)
       */
      val instance = LinearConstraintSet(
        Y * 1 -> Range(ClosedBound(0), NoBound),
        Y + X * 2 -> Range(NoBound, OpenBound(1)),
        Y - X * 2 -> Range(NoBound, OpenBound(1))
      )
      "it should never allow constraints that are horizontal, and purely below the bottom edge" in {
        instance.implies(Y * 2, Range(NoBound, ClosedBound(-1))) shouldEqual Never
        instance.implies(Y * 2, Range(NoBound, OpenBound(0))) shouldEqual Never
      }
      "it should never allow horizontal constraints above the triangle" in {
        instance.implies(Y * 3, Range(OpenBound(4), NoBound)) shouldEqual Never
        instance.implies(Y * 3, Range(ClosedBound(3), NoBound)) shouldEqual Never

      }
      "it should never allow constraints that don't overlap the triangle" in {
        instance.implies(X + Y, Range(ClosedBound(1), NoBound)) shouldEqual Never
      }
    }
  }
}
