package io.github.jamespic.ethereum_tools.decompiler.control_flow

import scala.collection.immutable.SortedSet

import org.scalatest.{FreeSpec, Matchers}

import io.github.jamespic.ethereum_tools._
import Bytecode._


class ControlGraphSpec extends FreeSpec with Matchers {
  def fakeBlock(address: Int, exitPoint: ExitPoint) = new BasicBlock(address, Nil, StateChange(exitPoint, StackState()))
  val instance = ControlGraph(SortedSet[Block](
    fakeBlock(0, Fallthrough(4)),
    fakeBlock(4, ConditionalExit(ConstJump(4), StackJump(2))),
    fakeBlock(6, CalculatedJump),
    fakeBlock(10, ConditionalExit(Halt, ConstJump(4))),
    fakeBlock(15, ConditionalExit(Halt, ConstJump(10))),
    fakeBlock(17, ConditionalExit(Throw, ConstJump(4)))
  ))

  "ControlGraphSpec" - {
    "should find parent blocks" in {
      instance.parents should equal(Map(
        0 -> SortedSet.empty[Block],
        4 -> SortedSet(
          fakeBlock(0, Fallthrough(4)),
          fakeBlock(4, ConditionalExit(ConstJump(4), StackJump(2))),
          fakeBlock(10, ConditionalExit(Halt, ConstJump(4))),
          fakeBlock(17, ConditionalExit(Throw, ConstJump(4)))
        ),
        6 -> SortedSet.empty[Block],
        10 -> SortedSet(fakeBlock(15, ConditionalExit(Halt, ConstJump(10)))),
        15 -> SortedSet.empty[Block],
        17 -> SortedSet.empty[Block]
      ))
    }
  }
}
