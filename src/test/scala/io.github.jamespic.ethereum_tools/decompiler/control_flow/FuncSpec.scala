package io.github.jamespic.ethereum_tools.decompiler.control_flow

import org.scalatest.{FreeSpec, Matchers}


class FuncSpec extends FreeSpec with Matchers {
  import Func._
  def block(address: Int, exitPoint: ExitPoint, stackState: StackState) = new BasicBlock(address, Nil, StateChange(exitPoint, stackState))
  "Func" - {
    "identifyFunctionsByReturn" - {
      "should identify simple functions" in {
        val a = block(0, ConstJump(2), StackState(List(ConstExpr(1), ConstExpr(4))))
        val b = block(2, StackJump(1), StackState(List(ConstExpr(5), ConstExpr(6)), 2))
        val c = block(4, Halt, StackState(Nil, 2))
        val graph = ControlGraph(
          a,
          b,
          c
        )
        val result = Func.identifyFunctionsByReturn(graph)
        result should equal(FuncInfo(
          Set(FuncEntry(2, 1, 2)),
          Set(SignatureHint(0, 1, 2, 4))
        ))
      }
    }
  }
}
