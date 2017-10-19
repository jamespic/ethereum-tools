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
      val existingConstraints = Map(
        Y.! -> Range(ClosedBound(0), NoBound),
        Y + X * 2 -> Range(NoBound, OpenBound(1)),
        Y - X * 2 -> Range(NoBound, OpenBound(1))
      )
      val instance = LinearConstraintSet(existingConstraints)
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
      "it should allow ranges that fully cover the triangle" in {
        instance.implies(X.!, Range(NoBound, NoBound)) shouldEqual Always
        instance.implies(X.!, Range(OpenBound(Rational(-1,2)), OpenBound(Rational(1, 2)))) shouldEqual Always
        instance.implies(Y.!, Range(NoBound, NoBound)) shouldEqual Always
        instance.implies(Y.!, Range(ClosedBound(0), OpenBound(1))) shouldEqual Always
      }
      "it should partially allow ranges that don't fully cover it" in {
        instance.implies(Y.!, Range(OpenBound(0), OpenBound(1))) shouldEqual Sometimes(
          Set(
            LinearConstraintSet(existingConstraints + (Y.! -> Range(OpenBound(0), OpenBound(1))))
          ),
          Set(
            LinearConstraintSet(existingConstraints + (Y.! -> Range(ClosedBound(0), ClosedBound(0))))
          )
        )
        instance.implies(X + Y, Range(OpenBound(0), OpenBound(Rational(1, 2)))) shouldEqual Sometimes(
          Set(
            LinearConstraintSet(existingConstraints + (X + Y -> Range(OpenBound(0), OpenBound(Rational(1, 2)))))
          ),
          Set(
            LinearConstraintSet(existingConstraints + (X + Y -> Range(OpenBound(Rational(-1, 2)), ClosedBound(0)))),
            LinearConstraintSet(existingConstraints + (X + Y -> Range(ClosedBound(Rational(1, 2)), OpenBound(1))))
          )
        )
      }
    }
  }
}
