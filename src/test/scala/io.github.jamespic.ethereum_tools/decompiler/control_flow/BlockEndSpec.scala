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
      BlockEnd.chain(a, b) should equal (c)
    }
    def shouldGive(c: ExitPoint): Unit = shouldGive(BlockEnd(exitPoint=c))
    def shouldGive(c: StackState): Unit = shouldGive(BlockEnd(stackState=c))
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
        merging(StackJump(3), StackJump(3)).shouldGive(StackJump(3))
      }
      "should refuse to unify incompatible simple exit points" in {
        merging(ConstJump(5), ConstJump(4)).shouldFail
        merging(Halt, FunctionReturn(3)).shouldFail
        merging(StackJump(2), StackJump(3)).shouldFail
      }
      "should unify anything with Throw" in {
        merging(ConstJump(4), Throw).shouldGive(ConstJump(4))
        merging(Throw, ConstJump(4)).shouldGive(ConstJump(4))
      }
      "should unify compatible exit points when one or more can return the contract early" in {
        merging(ConstJump(4), Halt).shouldGive(ConstJump(4))
      }
      "should unify compatible exit points when one or more can return the function early" in {
        merging(ConstJump(4), FunctionReturn(2)).shouldGive(ConstJump(4))
      }
      "should refuse to unify incompatible exit points even in conditional jumps" in {
        merging(ConstJump(3), ConditionalExit(Halt, ConstJump(4))).shouldFail
        merging(StackJump(3), ConditionalExit(FunctionReturn(2), StackJump(4))).shouldFail
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
        chaining(endA, BlockEnd(exitPoint=StackJump(0)))
          .shouldGive(BlockEnd(ConstJump(3), stackStateA))
        chaining(endA, BlockEnd(exitPoint=StackJump(1)))
          .shouldGive(BlockEnd(StackJump(2), stackStateA))
        chaining(endA, BlockEnd(exitPoint=StackJump(2)))
          .shouldGive(BlockEnd(CalculatedJump, stackStateA))
        chaining(endA, BlockEnd(exitPoint=StackJump(3)))
          .shouldGive(BlockEnd(StackJump(1), stackStateA))
      }
    }
  }
}
