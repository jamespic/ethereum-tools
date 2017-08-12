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
        val graph = ControlGraph(a, b, c)
        val result = Func.identifyFunctionsByReturn(graph)
        result should equal(FuncInfo(
          Set(FuncEntry(2, 1, 2)),
          Set(SignatureHint(0, 1, 2, 4))
        ))
      }
      "should identify functions within functions" in {
        val a = block(0, ConstJump(2), StackState(List(ConstExpr(8))))
        val b = block(2, ConstJump(4), StackState(List(ConstExpr(6))))
        val c = block(4, StackJump(0), StackState(List(CalculatedExpr), 1))
        val d = block(6, StackJump(1), StackState(List(ConstExpr(3)), 2))
        val e = block(8, Halt, StackState())
        val graph = ControlGraph(a, b, c, d, e)
        val result = Func.identifyFunctionsByReturn(graph)
        result should equal(FuncInfo(
          Set(FuncEntry(2, 0, 1), FuncEntry(4, 0, 1)),
          Set(SignatureHint(0, 0, 1, 8), SignatureHint(2, 0, 1, 6))
        ))
      }
      "should identify functions that are called in both branches of an if" in {
        val a = block(0, ConditionalExit(ConstJump(2), ConstJump(4)), StackState())
        val b = block(2, ConstJump(6), StackState(List(ConstExpr(8))))
        val c = block(4, ConstJump(6), StackState(List(ConstExpr(10))))
        val d = block(6, StackJump(0), StackState(List(CalculatedExpr), 1))
        val e = block(8, ConstJump(12), StackState(List(), 1))
        val f = block(10, ConstJump(12), StackState(List(), 1))
        val g = block(12, Halt, StackState())
        val graph = ControlGraph(a, b, c, d, e, f, g)
        val result = Func.identifyFunctionsByReturn(graph)
        result should equal(FuncInfo(
          Set(FuncEntry(6, 0, 1)),
          Set(SignatureHint(4, 0, 1, 10), SignatureHint(2, 0, 1, 8))
        ))
      }
    }
    "idenfityExtraFunctionsBySharing" - {
      "should find non-returning functions if they appear in two different functions" in {
        // Known function 1 - calls unknown function
        val a = block(0, ConstJump(8), StackState(List(ConstExpr(2))))
        val b = block(2, Halt, StackState())
        // Known function 2 - calls unknown function
        val c = block(4, ConstJump(8), StackState(List(ConstExpr(6))))
        val d = block(6, Halt, StackState())
        // Unknown function 1
        val e = block(8, Throw, StackState())
        val graph = ControlGraph(a, b, c, d, e)
        val result = Func.identifyExtraFunctionsBySharing(graph, Set(0, 4))
        result should equal(Set(8))
      }
    }
    "guessUnknownFunctionInfo" - {
      "should guess FuncEntries for non-returning functions" in {
        val a = block(4, ConstJump(6), StackState(Nil, 1))
        val b = block(6, Throw, StackState(List(ConstExpr(6)), 2))
        val c = block(8, ConditionalExit(ConstJump(2), Throw), StackState(Nil, 2))
        val d = block(2, Halt, StackState())
        val graph = ControlGraph(a, b, c, d)
        val result = Func.guessUnknownFuncInfo(graph, Set(2), Set(4, 8))
        result should equal(Set(FuncEntry(4, 2, 0), FuncEntry(8, 1, 0)))
      }
    }
  }
}
