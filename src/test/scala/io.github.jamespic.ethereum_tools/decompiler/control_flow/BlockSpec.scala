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
        35 -> JUMPDEST,
        36 -> POP
    )) should equal (SortedSet(
        BasicBlock(0, List(
          0 -> PUSH("0004"),
          3 -> JUMP
        ), 0, ConstJump(4)),
        BasicBlock(4, List(
          4 -> CALLER,
          5 -> POP,
          6 -> STOP
        ), 0, Halt),
        BasicBlock(7, List(
          7 -> INVALID
        ), 0, Halt),
        BasicBlock(8, List(
          8 -> PUSH("01"),
          10 -> PUSH("02"),
          12 -> RETURN
        ), 2, Halt),
        BasicBlock(13, List(
          13 -> REVERT
        ), -2, Halt),
        BasicBlock(14, List(
          14 -> UNKNOWN
        ), 0, Halt),
        BasicBlock(15, List(
          15 -> PUSH("01"),
          17 -> PUSH("00"),
          19 -> JUMPI
        ), 0, ConditionalExit(ConstJump(0), ConstJump(20))),
        BasicBlock(20, List(
          20 -> PUSH("0100"),
          23 -> MLOAD,
          24 -> JUMP
        ), 0, CalculatedJump),
        BasicBlock(25, List(
          25 -> SWAP(1),
          26 -> JUMPI
        ), -2, ConditionalExit(FunctionReturn(1), ConstJump(27))),
        BasicBlock(27, List(
          27 -> PUSH("0100"),
          30 -> MLOAD,
          31 -> JUMPI
        ), -1, ConditionalExit(CalculatedJump, ConstJump(32))),
        BasicBlock(32, List(
          32 -> SWAP(2),
          33 -> JUMPI
        ), -2, FunctionReturn(2)),
        BasicBlock(34, List(
          34 -> POP
        ), -1, ConstJump(35)),
        BasicBlock(35, List(
          35 -> JUMPDEST,
          36 -> POP
        ), -1, Halt)
      ))
    }
  }
}
