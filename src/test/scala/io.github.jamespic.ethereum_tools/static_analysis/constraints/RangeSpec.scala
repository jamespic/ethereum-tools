package io.github.jamespic.ethereum_tools.static_analysis.constraints

import org.scalatest.{FreeSpec, Matchers}

class RangeSpec extends FreeSpec with Matchers {
  "RangeSpec" - {
    "addition" - {
      "should offset ranges" in {
        (Range(OpenBound(1), ClosedBound(3)) + 4) shouldEqual Range(OpenBound(5), ClosedBound(7))
        (Range(NoBound, OpenBound(8)) + 1) shouldEqual Range(NoBound, OpenBound(9))
        (Range(ClosedBound(3), NoBound) + 1) shouldEqual Range(ClosedBound(4), NoBound)
      }
    }
    "subtraction" - {
      "should offset ranges" in {
        (Range(OpenBound(1), ClosedBound(3)) - 4) shouldEqual Range(OpenBound(-3), ClosedBound(-1))
        (Range(NoBound, OpenBound(8)) - 1) shouldEqual Range(NoBound, OpenBound(7))
        (Range(ClosedBound(3), NoBound) - 1) shouldEqual Range(ClosedBound(2), NoBound)
      }
    }
    "multiplication" - {
      "should expand positive multipliers" in {
        (Range(OpenBound(1), ClosedBound(3)) * 3) shouldEqual Range(OpenBound(3), ClosedBound(9))
        (Range(NoBound, OpenBound(8)) * Rational(1, 2)) shouldEqual Range(NoBound, OpenBound(4))
        (Range(ClosedBound(3), NoBound) * 2) shouldEqual Range(ClosedBound(6), NoBound)
      }
      "should reverse negative multipliers" in {
        (Range(OpenBound(1), ClosedBound(3)) * (-3)) shouldEqual Range(ClosedBound(-9), OpenBound(-3))
        (Range(NoBound, OpenBound(8)) * Rational(-1, 2)) shouldEqual Range(OpenBound(-4), NoBound)
        (Range(ClosedBound(3), NoBound) * (-2)) shouldEqual Range(NoBound, ClosedBound(-6))
      }
    }
    "division" - {
      "should contract positive divisors" in {
        (Range(OpenBound(3), ClosedBound(9)) / 3) shouldEqual Range(OpenBound(1), ClosedBound(3))
        (Range(NoBound, OpenBound(8)) / 2) shouldEqual Range(NoBound, OpenBound(4))
        (Range(ClosedBound(6), NoBound) / 3) shouldEqual Range(ClosedBound(2), NoBound)
      }
      "should reverse negative divisors" in {
        (Range(OpenBound(-3), ClosedBound(9)) / (-3)) shouldEqual Range(ClosedBound(-3), OpenBound(1))
        (Range(NoBound, OpenBound(6)) / (-2)) shouldEqual Range(OpenBound(-3), NoBound)
        (Range(ClosedBound(-8), NoBound) / (-2)) shouldEqual Range(NoBound, ClosedBound(4))
      }
    }
    "implies" - {
      "should return Always" - {
        "when a equals b" in {
          Range(OpenBound(0), OpenBound(1)).implies(Range(OpenBound(0), OpenBound(1))) should equal(Always)
          Range(ClosedBound(0), ClosedBound(1)).implies(Range(ClosedBound(0), ClosedBound(1))) should equal(Always)
          Range(NoBound, NoBound).implies(Range(NoBound, NoBound)) should equal(Always)
        }
        "when a is a subset of b of the same type" in {
          Range(OpenBound(0), OpenBound(3)).implies(Range(OpenBound(0), OpenBound(4))) should equal(Always)
          Range(OpenBound(1), OpenBound(4)).implies(Range(OpenBound(0), OpenBound(4))) should equal(Always)
          Range(ClosedBound(0), ClosedBound(3)).implies(Range(ClosedBound(0), ClosedBound(4))) should equal(Always)
          Range(ClosedBound(1), ClosedBound(4)).implies(Range(ClosedBound(0), ClosedBound(4))) should equal(Always)
        }
        "when a is a subset of b of a different type" in {
          Range(OpenBound(0), OpenBound(4)).implies(Range(ClosedBound(0), OpenBound(4))) should equal(Always)
          Range(OpenBound(0), OpenBound(4)).implies(Range(OpenBound(0), ClosedBound(4))) should equal(Always)
          Range(ClosedBound(0), ClosedBound(3)).implies(Range(ClosedBound(0), OpenBound(4))) should equal(Always)
          Range(ClosedBound(1), ClosedBound(4)).implies(Range(OpenBound(0), ClosedBound(4))) should equal(Always)
          Range(OpenBound(0), OpenBound(4)).implies(Range(NoBound, OpenBound(4))) should equal(Always)
          Range(OpenBound(0), OpenBound(4)).implies(Range(OpenBound(0), NoBound)) should equal(Always)
          Range(ClosedBound(0), ClosedBound(3)).implies(Range(ClosedBound(0), NoBound)) should equal(Always)
          Range(ClosedBound(1), ClosedBound(4)).implies(Range(NoBound, ClosedBound(4))) should equal(Always)
        }
      }
      "should return Sometimes" - {
        "when a and b have different bounds" in {
          Range(OpenBound(0), ClosedBound(2)).implies(Range(OpenBound(1), ClosedBound(3))) should equal(Sometimes(
            Set(Range(OpenBound(1), ClosedBound(2))),
            Set(Range(OpenBound(0), ClosedBound(1)))
          ))
          Range(OpenBound(1), ClosedBound(3)).implies(Range(OpenBound(0), ClosedBound(2))) should equal(Sometimes(
            Set(Range(OpenBound(1), ClosedBound(2))),
            Set(Range(OpenBound(2), ClosedBound(3)))
          ))
          Range(NoBound, NoBound).implies(Range(ClosedBound(0), OpenBound(1))) should equal(Sometimes(
            Set(Range(ClosedBound(0), OpenBound(1))),
            Set(
              Range(NoBound, OpenBound(0)),
              Range(ClosedBound(1), NoBound)
            )
          ))
        }
        "when a and be have the same bound, but a is closed and b is open" in {
          Range(ClosedBound(0), OpenBound(1)).implies(Range(OpenBound(0), OpenBound(1))) should equal(Sometimes(
            Set(Range(OpenBound(0), OpenBound(1))),
            Set(Range(ClosedBound(0), ClosedBound(0)))
          ))
          Range(OpenBound(0), ClosedBound(1)).implies(Range(OpenBound(0), OpenBound(1))) should equal(Sometimes(
            Set(Range(OpenBound(0), OpenBound(1))),
            Set(Range(ClosedBound(1), ClosedBound(1)))
          ))
          Range(ClosedBound(0), ClosedBound(1)).implies(Range(OpenBound(0), OpenBound(1))) should equal(Sometimes(
            Set(Range(OpenBound(0), OpenBound(1))),
            Set(
              Range(ClosedBound(0), ClosedBound(0)),
              Range(ClosedBound(1), ClosedBound(1))
            )
          ))
        }
        "when the two ranges only meet at one point, due to closed bounds" in {
          Range(ClosedBound(0), ClosedBound(1)).implies(Range(ClosedBound(1), ClosedBound(2))) should equal(Sometimes(
            Set(Range(ClosedBound(1), ClosedBound(1))),
            Set(Range(ClosedBound(0), OpenBound(1)))
          ))
          Range(ClosedBound(1), ClosedBound(2)).implies(Range(ClosedBound(0), ClosedBound(1))) should equal(Sometimes(
            Set(Range(ClosedBound(1), ClosedBound(1))),
            Set(Range(OpenBound(1), ClosedBound(2)))
          ))
        }
      }
      "should return Never" - {
        "when two ranges meet at an open bound" in {
          Range(NoBound, OpenBound(0)).implies(Range(OpenBound(0), NoBound)) should equal(Never)
          Range(OpenBound(0), NoBound).implies(Range(NoBound, OpenBound(0))) should equal(Never)
        }
        "when two ranges meet and one is open the other closed bound" in {
          Range(NoBound, OpenBound(0)).implies(Range(ClosedBound(0), NoBound)) should equal(Never)
          Range(NoBound, ClosedBound(0)).implies(Range(OpenBound(0), NoBound)) should equal(Never)
          Range(OpenBound(0), NoBound).implies(Range(NoBound, ClosedBound(0))) should equal(Never)
          Range(ClosedBound(0), NoBound).implies(Range(NoBound, OpenBound(0))) should equal(Never)
        }
        "when one range's bound is strictly larger than the other" in {
          Range(NoBound, OpenBound(0)).implies(Range(ClosedBound(1), NoBound)) should equal(Never)
          Range(NoBound, ClosedBound(0)).implies(Range(OpenBound(1), NoBound)) should equal(Never)
          Range(OpenBound(1), NoBound).implies(Range(NoBound, ClosedBound(0))) should equal(Never)
          Range(ClosedBound(1), NoBound).implies(Range(NoBound, OpenBound(0))) should equal(Never)
        }
      }
    }
  }
}
