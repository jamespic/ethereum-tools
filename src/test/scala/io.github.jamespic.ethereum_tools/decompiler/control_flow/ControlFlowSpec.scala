package io.github.jamespic.ethereum_tools.decompiler.control_flow

import org.scalatest.{FreeSpec, Matchers}

import io.github.jamespic.ethereum_tools._
import Bytecode._


class DecompilerSpec extends FreeSpec with Matchers {
  "identifyBasicBlocks" - {
    "should assemble instruction lists into sublists" in {
      identifyBasicBlocks(List(
        0 -> PUSH("0004"),
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
        19 -> JUMPI
      )) should equal (Map(
        0 -> List(
          0 -> PUSH("0004"),
          3 -> JUMP
        ),
        4 -> List(
          4 -> CALLER,
          5 -> POP,
          6 -> STOP
        ),
        7 -> List(
          7 -> INVALID
        ),
        8 -> List(
          8 -> PUSH("01"),
          10 -> PUSH("02"),
          12 -> RETURN
        ),
        13 -> List(
          13 -> REVERT
        ),
        14 -> List(
          14 -> UNKNOWN
        ),
        15 -> List(
          15 -> PUSH("01"),
          17 -> PUSH("00"),
          19 -> JUMPI
        )
      ))

    }
  }
}
