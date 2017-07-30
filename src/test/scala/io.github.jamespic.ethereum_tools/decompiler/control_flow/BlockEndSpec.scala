package io.github.jamespic.ethereum_tools.decompiler.control_flow

import org.scalatest.{FreeSpec, Matchers}



class BlockEndSpec extends FreeSpec with Matchers {
  case class merging(a: BlockEnd, b: BlockEnd) {
    def shouldGive(c: BlockEnd): Unit = {
      BlockEnd.merge(a, b) should equal (Some(c))
      BlockEnd.merge(b, a) should equal (Some(c))
    }
    def shouldGive(c: ExitPoint): Unit = shouldGive(BlockEnd(exitPoint=c))
    def shouldGive(c: StackState): Unit = shouldGive(BlockEnd(stackState=c))
    def shouldFail = {
      BlockEnd.merge(a, b) should equal (None)
      BlockEnd.merge(b, a) should equal (None)
    }
  }

  object chaining {
    def apply(a: ExitPoint, b: ExitPoint): chaining = this(BlockEnd(exitPoint=a), BlockEnd(exitPoint=b))
    def apply(a: StackState, b: StackState): chaining = this(BlockEnd(stackState=a), BlockEnd(stackState=b))
  }

  case class chaining(a: BlockEnd, b: BlockEnd) {
    def shouldGive(c: BlockEnd): Unit = {
      BlockEnd.chain(a, b) should equal (Some(c))
    }
    def shouldGive(c: ExitPoint): Unit = shouldGive(BlockEnd(exitPoint=c))
    def shouldGive(c: StackState): Unit = shouldGive(BlockEnd(stackState=c))
    def shouldFail = {
      BlockEnd.chain(a, b) should equal (None)
    }
  }

  object merging {
    def apply(a: ExitPoint, b: ExitPoint): merging = this(BlockEnd(exitPoint=a), BlockEnd(exitPoint=b))
    def apply(a: StackState, b: StackState): merging = this(BlockEnd(stackState=a), BlockEnd(stackState=b))
  }

  "BlockEnd" - {
    "unify" - {
      "should never unify CalculatedJump, even with itself" in {
        merging(CalculatedJump, CalculatedJump).shouldFail
      }
      "should always unify alike exit points" in {
        merging(ConstJump(5), ConstJump(5)).shouldGive(ConstJump(5))
        merging(FunctionReturn(3), FunctionReturn(3)).shouldGive(FunctionReturn(3))
      }
      "should refuse to unify incompatible simple exit points" in {
        merging(ConstJump(5), ConstJump(4)).shouldFail
        merging(Halt, FunctionReturn(3)).shouldFail
        merging(FunctionReturn(2), FunctionReturn(3)).shouldFail
      }
      "should unify anything with Throw" in {
        merging(ConstJump(4), Throw).shouldGive(ConstJump(4))
        merging(Throw, ConstJump(4)).shouldGive(ConstJump(4))
      }
      "should unify compatible exit points when one or more can return the contract early" in {
        merging(ConstJump(4), Halt).shouldGive(WithEarlyContractReturn(ConstJump(4)))
        merging(ConstJump(4), WithEarlyContractReturn(ConstJump(4))).shouldGive(WithEarlyContractReturn(ConstJump(4)))
        merging(ConstJump(4), WithEarlyContractReturn(Throw)).shouldGive(WithEarlyContractReturn(ConstJump(4)))
        merging(Halt, WithEarlyContractReturn(ConstJump(4))).shouldGive(WithEarlyContractReturn(ConstJump(4)))
      }
      "should refuse to unify contract return incompatible exit points with early contract return ones" in {
        merging(FunctionReturn(2), WithEarlyContractReturn(ConstJump(4))).shouldFail
        merging(WithEarlyFunctionReturn(2, ConstJump(4)), WithEarlyContractReturn(ConstJump(4))).shouldFail
      }
      "should unify compatible exit points when one or more can return the function early" in {
        merging(ConstJump(4), FunctionReturn(2)).shouldGive(WithEarlyFunctionReturn(2, ConstJump(4)))
        merging(ConstJump(4), WithEarlyFunctionReturn(2, ConstJump(4))).shouldGive(WithEarlyFunctionReturn(2, ConstJump(4)))
        merging(ConstJump(4), WithEarlyFunctionReturn(2, Throw)).shouldGive(WithEarlyFunctionReturn(2, ConstJump(4)))
        merging(FunctionReturn(2), WithEarlyFunctionReturn(2, ConstJump(4))).shouldGive(WithEarlyFunctionReturn(2, ConstJump(4)))
      }
      "should refuse to unify function return incompatible exit points with early function return ones" in {
        merging(Halt, WithEarlyFunctionReturn(2, ConstJump(4))).shouldFail
        merging(WithEarlyFunctionReturn(2, ConstJump(4)), WithEarlyFunctionReturn(3, ConstJump(4))).shouldFail
      }
      "should refuse to unify otherwise imcompatible exit points within wrappers" in {
        merging(WithEarlyContractReturn(ConstJump(2)), WithEarlyContractReturn(ConstJump(4))).shouldFail
        merging(WithEarlyFunctionReturn(4, ConstJump(2)), WithEarlyFunctionReturn(4, ConstJump(4))).shouldFail
      }
      "should refuse to unify function return incompatible exit points even in conditional jumps" in {
        merging(FunctionReturn(2), ConditionalExit(Halt, ConstJump(4))).shouldFail
        merging(WithEarlyFunctionReturn(2, ConstJump(4)), ConditionalExit(Halt, ConstJump(4))).shouldFail
      }
      "should refuse to unify contract return incompatible exit points even in conditional jumps" in {
        merging(Halt, ConditionalExit(FunctionReturn(2), ConstJump(4))).shouldFail
        merging(WithEarlyContractReturn(ConstJump(4)), ConditionalExit(FunctionReturn(2), ConstJump(4))).shouldFail
      }
      "should combine compatible elements from two merged stacks" in {
        val stack1 = StackState(
          List(
            StackVar(3),
            ConstExpr(3),
            StackVar(2),
            CalculatedExpr,
            StackVar(1)
          ), 2
        )
        val stack2 = StackState(
          List(
            StackVar(2),
            ConstExpr(3),
            StackVar(2),
            StackVar(0)
          ), 1
        )
        val expected = StackState(
          List(
            CalculatedExpr,
            ConstExpr(3),
            StackVar(2),
            CalculatedExpr,
            StackVar(1)
          ), 2
        )
        merging(stack1, stack2).shouldGive(expected)
      }
    }
    "chain" - {
      "should map values from an old stack onto a new one" in {
        val stack1 = StackState(
          List(
            StackVar(0),
            ConstExpr(1),
            CalculatedExpr,
            StackVar(0)
          ),
          4
        )
        val stack2 = StackState(
          List(
            ConstExpr(3),
            StackVar(1),
            StackVar(0)
          ),
          2
        )
        val expected = StackState(
          List(
            ConstExpr(3),
            ConstExpr(1),
            StackVar(0),
            CalculatedExpr,
            StackVar(0)
          ),
          4
        )
        chaining(stack1, stack2).shouldGive(expected)
      }
      "should chain compatible exit codes together" in {
        chaining(ConstJump(1), ConstJump(2)).shouldGive(ConstJump(2))

      }
      "should propagate early contract return codes" in {
        chaining(WithEarlyContractReturn(ConstJump(1)), ConstJump(2))
          .shouldGive(WithEarlyContractReturn(ConstJump(2)))
        chaining(WithEarlyContractReturn(ConstJump(1)), WithEarlyContractReturn(ConstJump(2)))
          .shouldGive(WithEarlyContractReturn(ConstJump(2)))
        chaining(WithEarlyContractReturn(ConstJump(1)), ConditionalExit(ConstJump(4), ConstJump(5)))
          .shouldGive(WithEarlyContractReturn(ConditionalExit(ConstJump(4), ConstJump(5))))
        chaining(WithEarlyContractReturn(ConstJump(1)), Halt)
          .shouldGive(Halt)
      }
      "should propagate early function return codes" in {
        chaining(WithEarlyFunctionReturn(4, ConstJump(1)), ConstJump(2))
          .shouldGive(WithEarlyFunctionReturn(4, ConstJump(2)))
        chaining(WithEarlyFunctionReturn(4, ConstJump(1)), WithEarlyFunctionReturn(4, ConstJump(2)))
          .shouldGive(WithEarlyFunctionReturn(4, ConstJump(2)))
        chaining(WithEarlyFunctionReturn(6, ConstJump(1)), ConditionalExit(ConstJump(4), ConstJump(5)))
          .shouldGive(WithEarlyFunctionReturn(6, ConditionalExit(ConstJump(4), ConstJump(5))))
        chaining(WithEarlyFunctionReturn(6, ConstJump(1)), FunctionReturn(6))
          .shouldGive(FunctionReturn(6))
      }
      "should refuse to chain unsafe combinations" in {
        chaining(WithEarlyContractReturn(ConstJump(1)), FunctionReturn(2)).shouldFail
        chaining(WithEarlyFunctionReturn(6, ConstJump(1)), Halt).shouldFail
        chaining(WithEarlyFunctionReturn(6, ConstJump(1)), WithEarlyContractReturn(ConstJump(2))).shouldFail
        chaining(WithEarlyFunctionReturn(6, ConstJump(1)), ConditionalExit(Halt, ConstJump(5))).shouldFail
        chaining(WithEarlyContractReturn(ConstJump(1)), ConditionalExit(FunctionReturn(2), ConstJump(5))).shouldFail
      }
      "should fix up function return values" in {
        val stackStateA = StackState(
          List(
            ConstExpr(3),
            StackVar(2),
            CalculatedExpr
          ), 1
        )
        val endA = BlockEnd(
          stackState = stackStateA
        )
        chaining(endA, BlockEnd(exitPoint=FunctionReturn(0)))
          .shouldGive(BlockEnd(ConstJump(3), stackStateA))
        chaining(endA, BlockEnd(exitPoint=FunctionReturn(1)))
          .shouldGive(BlockEnd(FunctionReturn(2), stackStateA))
        chaining(endA, BlockEnd(exitPoint=FunctionReturn(2)))
          .shouldGive(BlockEnd(CalculatedJump, stackStateA))
        chaining(endA, BlockEnd(exitPoint=FunctionReturn(3)))
          .shouldGive(BlockEnd(FunctionReturn(1), stackStateA))
      }
    }
  }
}
