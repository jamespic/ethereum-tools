package io.github.jamespic.ethereum_tools.decompiler.control_flow

import org.scalatest.{FreeSpec, Matchers}

class ExitPointSpec extends FreeSpec with Matchers {
  case class unifying(a: ExitPoint, b: ExitPoint) {
    def shouldGive(c: ExitPoint) = {
      ExitPoint.unify(a, b) should equal (Some(c))
      ExitPoint.unify(b, a) should equal (Some(c))
    }
    def shouldFail = {
      ExitPoint.unify(a, b) should equal (None)
      ExitPoint.unify(b, a) should equal (None)
    }
  }
  "ExitPoint" - {
    "unify" - {
      "should never unify CalculatedJump, even with itself" in {
        unifying(CalculatedJump, CalculatedJump).shouldFail
      }
      "should always unify alike exit points" in {
        unifying(ConstJump(5), ConstJump(5)).shouldGive(ConstJump(5))
        unifying(FunctionReturn(3), FunctionReturn(3)).shouldGive(FunctionReturn(3))
      }
      "should refuse to unify incompatible simple exit points" in {
        unifying(ConstJump(5), ConstJump(4)).shouldFail
        unifying(Halt, FunctionReturn(3)).shouldFail
        unifying(FunctionReturn(2), FunctionReturn(3)).shouldFail
      }
      "should unify anything with Throw" in {
        unifying(ConstJump(4), Throw).shouldGive(ConstJump(4))
        unifying(Throw, ConstJump(4)).shouldGive(ConstJump(4))
      }
      "should unify compatible exit points when one or more can return the contract early" in {
        unifying(ConstJump(4), Halt).shouldGive(WithEarlyContractReturn(ConstJump(4)))
        unifying(ConstJump(4), WithEarlyContractReturn(ConstJump(4))).shouldGive(WithEarlyContractReturn(ConstJump(4)))
        unifying(ConstJump(4), WithEarlyContractReturn(Throw)).shouldGive(WithEarlyContractReturn(ConstJump(4)))
        unifying(Halt, WithEarlyContractReturn(ConstJump(4))).shouldGive(WithEarlyContractReturn(ConstJump(4)))
      }
      "should refuse to unify contract return incompatible exit points with early contract return ones" in {
        unifying(FunctionReturn(2), WithEarlyContractReturn(ConstJump(4))).shouldFail
        unifying(WithEarlyFunctionReturn(2, ConstJump(4)), WithEarlyContractReturn(ConstJump(4))).shouldFail
      }
      "should unify compatible exit points when one or more can return the function early" in {
        unifying(ConstJump(4), FunctionReturn(2)).shouldGive(WithEarlyFunctionReturn(2, ConstJump(4)))
        unifying(ConstJump(4), WithEarlyFunctionReturn(2, ConstJump(4))).shouldGive(WithEarlyFunctionReturn(2, ConstJump(4)))
        unifying(ConstJump(4), WithEarlyFunctionReturn(2, Throw)).shouldGive(WithEarlyFunctionReturn(2, ConstJump(4)))
        unifying(FunctionReturn(2), WithEarlyFunctionReturn(2, ConstJump(4))).shouldGive(WithEarlyFunctionReturn(2, ConstJump(4)))
      }
      "should refuse to unify function return incompatible exit points with early function return ones" in {
        unifying(Halt, WithEarlyFunctionReturn(2, ConstJump(4))).shouldFail
        unifying(WithEarlyFunctionReturn(2, ConstJump(4)), WithEarlyFunctionReturn(3, ConstJump(4))).shouldFail
      }
      "should refuse to unify otherwise imcompatible exit points within wrappers" in {
        unifying(WithEarlyContractReturn(ConstJump(2)), WithEarlyContractReturn(ConstJump(4))).shouldFail
        unifying(WithEarlyFunctionReturn(4, ConstJump(2)), WithEarlyFunctionReturn(4, ConstJump(4))).shouldFail
      }
      "should refuse to unify function return incompatible exit points even in conditional jumps" in {
        unifying(FunctionReturn(2), ConditionalExit(Halt, ConstJump(4))).shouldFail
        unifying(WithEarlyFunctionReturn(2, ConstJump(4)), ConditionalExit(Halt, ConstJump(4))).shouldFail
      }
      "should refuse to unify contract return incompatible exit points even in conditional jumps" in {
        unifying(Halt, ConditionalExit(FunctionReturn(2), ConstJump(4))).shouldFail
        unifying(WithEarlyContractReturn(ConstJump(4)), ConditionalExit(FunctionReturn(2), ConstJump(4))).shouldFail
      }
    }
  }
}
