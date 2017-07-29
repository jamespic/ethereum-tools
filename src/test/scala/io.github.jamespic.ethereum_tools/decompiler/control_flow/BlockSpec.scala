package io.github.jamespic.ethereum_tools.decompiler.control_flow

import org.scalatest.{FreeSpec, Matchers}
import scala.collection.immutable.SortedSet

import io.github.jamespic.ethereum_tools._
import Bytecode._


class BlockSpec extends FreeSpec with Matchers {
  "identifyBasicBlocks" - {
    "should assemble instruction lists into sublists" in {
      // FIXME This needs rewriting following the Block code refactor
      Block.identifyBasicBlocks(List(
        0 -> PUSH("0008"),
        3 -> JUMP,

        4 -> CALLER,
        5 -> POP,
        6 -> STOP,

        7 -> INVALID,

        8 -> PUSH("01"),
        10 -> PUSH("02"),
        12 -> RETURN,

        13 -> REVERT,

        14 -> UNKNOWN,

        15 -> PUSH("01"),
        17 -> PUSH("00"),
        19 -> JUMPI,

        20 -> PUSH("0100"),
        23 -> MLOAD,
        24 -> JUMP,

        25 -> SWAP(1),
        26 -> JUMPI,

        27 -> PUSH("0100"),
        30 -> MLOAD,
        31 -> JUMPI,

        32 -> SWAP(2),
        33 -> JUMPI,

        34 -> POP,
        35 -> JUMP,

        36 -> POP,

        37 -> JUMPDEST,
        38 -> POP
    )) should equal (SortedSet(
        BasicBlock(0, List(
          0 -> PUSH("0004"),
          3 -> JUMP
        ), StackState(), ConstJump(4)),
        BasicBlock(4, List(
          4 -> CALLER,
          5 -> POP,
          6 -> STOP
        ), StackState(), Halt),
        BasicBlock(7, List(
          7 -> INVALID
        ), StackState(), Throw),
        BasicBlock(8, List(
          8 -> PUSH("01"),
          10 -> PUSH("02"),
          12 -> RETURN
        ), StackState(List(ConstExpr(2), ConstExpr(1))), Halt),
        BasicBlock(13, List(
          13 -> REVERT
        ), StackState(Nil, 2), Throw),
        BasicBlock(14, List(
          14 -> UNKNOWN
        ), StackState(), Throw),
        BasicBlock(15, List(
          15 -> PUSH("01"),
          17 -> PUSH("00"),
          19 -> JUMPI
        ), StackState(), ConditionalExit(ConstJump(0), ConstJump(20))),
        BasicBlock(20, List(
          20 -> PUSH("0100"),
          23 -> MLOAD,
          24 -> JUMP
        ), StackState(), CalculatedJump),
        BasicBlock(25, List(
          25 -> SWAP(1),
          26 -> JUMPI
        ), StackState(Nil, 2), ConditionalExit(FunctionReturn(1), ConstJump(27))),
        BasicBlock(27, List(
          27 -> PUSH("0100"),
          30 -> MLOAD,
          31 -> JUMPI
        ), StackState(Nil, 1), ConditionalExit(CalculatedJump, ConstJump(32))),
        BasicBlock(32, List(
          32 -> SWAP(2),
          33 -> JUMPI
        ), StackState(Nil, 2), FunctionReturn(2)),
        BasicBlock(34, List(
          34 -> POP,
          35 -> JUMP
        ), StackState(Nil, 2), FunctionReturn(2)),
        BasicBlock(36, List(
          36 -> POP
        ), StackState(Nil,1), ConstJump(35)),
        BasicBlock(37, List(
          37 -> JUMPDEST,
          38 -> POP
        ), StackState(Nil, 1), Halt)
      ))
    }
  }
}
