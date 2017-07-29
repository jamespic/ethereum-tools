package io.github.jamespic.ethereum_tools.decompiler.control_flow

import org.scalatest.{FreeSpec, Matchers}

import io.github.jamespic.ethereum_tools._
import Bytecode._


class StackStateSpec extends FreeSpec with Matchers {
  "StackState" - {
    "pop" - {
      "should return a new variable for an unpopulated stack" in {
        val stack = StackState(Nil, 4)
        val (expr, newStack) = stack.pop
        newStack should equal (StackState(Nil, 5))
        expr should equal (StackVar(4))
      }
      "should return multiple variables when requested" in {
        val stack = StackState(List(ConstExpr(6), CalculatedExpr), -2)
        val (exprs, newStack) = stack.pop(4)
        newStack should equal (StackState(Nil, 0))
        exprs should equal (List(ConstExpr(6), CalculatedExpr, StackVar(-2), StackVar(-1)))
      }
    }
    "push" - {
      "should push elements onto the front of the stack" in {
        val stack = StackState(List(ConstExpr(7)), 5)
        val newStack = stack.push(CalculatedExpr, StackVar(3))
        newStack should equal (StackState(List(CalculatedExpr, StackVar(3), ConstExpr(7)), 5))
      }
    }
    "height" - {
      "should measure the height of the stack above 'base'" in {
        val stack = StackState(List(ConstExpr(1), StackVar(4)), 2)
        stack.height should equal (0)
      }
    }
    "progress" - {
      "should push constants on PUSH" in {
        val stack = StackState()
        stack.progress(PUSH("20")) should equal (StackState(List(ConstExpr(0x20))))
      }
      "should cycle variables on SWAP" in {
        val stack = StackState()
        stack.progress(SWAP(3)) should equal (
          StackState(
            List(StackVar(3), StackVar(0), StackVar(1), StackVar(2)),
            4
          )
        )
      }
      "should copy variables on DUP" in {
        val stack = StackState()
        stack.progress(DUP(2)) should equal (
          StackState(
            List(StackVar(1), StackVar(0), StackVar(1)),
            2
          )
        )
      }
      "should add calculated expressions to the stack for other ops" in {
        val stack = StackState(List(ConstExpr(5)), 2)
        stack.progress(ADD) should equal (
          StackState(List(CalculatedExpr), 3)
        )
      }
    }
  }
}
