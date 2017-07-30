package io.github.jamespic.ethereum_tools.decompiler.control_flow

import org.scalatest.{FreeSpec, Matchers}

class GraphRewriteRulesSpec extends FreeSpec with Matchers {
  import GraphRewriteRules._
  "GraphRewriteRules" - {
    "findIfRewrite" - {
      "should rewrite simple if blocks" in {
        val a = BasicBlock(0, Nil, BlockEnd(ConditionalExit(ConstJump(1), ConstJump(2)), StackState()))
        val b = BasicBlock(1, Nil, BlockEnd(ConstJump(2), StackState()))
        val c = BasicBlock(2, Nil, BlockEnd(Halt, StackState()))
        val graph = ControlGraph(a, b, c)
        val rewritten = findIfRewrite(graph)
        rewritten should equal (Some(
          ControlGraph(
            IfBlock(0,
              a,
              b,
              BlockEnd(
                ConstJump(2),
                StackState()
              )
            ),
            c
          )
        ))
      }
      "should fix up return addresses" in {
        val a = BasicBlock(0, Nil, BlockEnd(ConditionalExit(ConstJump(1), FunctionReturn(1)), StackState(List(ConstExpr(1)))))
        val b = BasicBlock(1, Nil, BlockEnd(FunctionReturn(2), StackState()))

        val graph = ControlGraph(
          a, b
        )
        val rewritten = findIfRewrite(graph)
        rewritten should equal (Some(
          ControlGraph(
            IfBlock(0,
              a,
              b,
              BlockEnd(
                FunctionReturn(1),
                StackState(List(ConstExpr(1)))
              )
            )
          )
        ))
      }
    }
    "findUnlessRewrite" - {
      "should rewrite simple unless blocks" in {
        val a = BasicBlock(0, Nil, BlockEnd(ConditionalExit(ConstJump(2), ConstJump(1)), StackState()))
        val b = BasicBlock(1, Nil, BlockEnd(ConstJump(2), StackState()))
        val c = BasicBlock(2, Nil, BlockEnd(Halt, StackState()))
        val graph = ControlGraph(a, b, c)
        val rewritten = findUnlessRewrite(graph)
        rewritten should equal (Some(
          ControlGraph(
            UnlessBlock(0,
              a,
              b,
              BlockEnd(
                ConstJump(2),
                StackState()
              )
            ),
            c
          )
        ))
      }
    }
  }
}
