package io.github.jamespic.ethereum_tools.static_analysis.constraints

import org.scalatest.{FreeSpec, Matchers}

class MemorySpec extends FreeSpec with Matchers {
  "Rational" - {
    "constructor" - {
      "should eliminate common factors" in {
        Rational(2, 6) shouldEqual Rational(1, 3)
      }
      "should make denominators positive" in {
        Rational(1, -1) shouldEqual Rational(-1, 1)
      }
    }
    "addition" - {
      "should handle different denominators" in {
        (Rational(1, 3) + Rational(1, 2)) shouldEqual Rational(5, 6)
      }
      "should work correctly with the same denominator" in {
        (Rational(1, 4) + Rational(2, 4)) shouldEqual Rational(3, 4)
      }
    }
    "multiplication" - {
      "should resolve common factors" in {
        (Rational(2, 21) * Rational(3, 5)) shouldEqual Rational(2, 35)
      }
    }
    "division" - {
      "should resolve common factors" in {
        (Rational(2, 21) / Rational(5, 3)) shouldEqual Rational(2, 35)
      }
    }
    "compare" - {
      "should compare stuff" in {
        Rational(1, 3) should be < Rational(1, 2)
        Rational(1, 2) should be <= Rational(1, 2)
      }
    }
    "equality" - {
      "should distinguish different things" in {
        Rational(1, 3) should not equal Rational(1, 2)
        Rational(3, 2) should not equal Rational(1, 2)
        Rational(3, 3) should not equal Rational(1, 2)
      }
    }
  }
}