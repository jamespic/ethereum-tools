package io.github.jamespic.ethereum_tools.decompiler.control_flow

import org.scalatest.{FreeSpec, Matchers}

class GraphRewriteRulesSpec extends FreeSpec with Matchers {
  import GraphRewriteRules._
  "GraphRewriteRules" - {
    "findIfRewrite" - {
      "should rewrite simple if blocks" in {
        val graph = ControlGraph(
          BasicBlock(0, Nil, StackState(), ConditionalExit(ConstJump(1), ConstJump(2))),
          BasicBlock(1, Nil, StackState(), ConstJump(2)),
          BasicBlock(2, Nil, StackState(), Halt)
        )
        val rewritten = findIfRewrite(graph)
        rewritten should equal (Some(
          ControlGraph(
            IfBlock(0,
              BasicBlock(0, Nil, StackState(), ConditionalExit(ConstJump(1), ConstJump(2))),
              BasicBlock(1, Nil, StackState(), ConstJump(2)),
              StackState(),
              ConstJump(2)
            ),
            BasicBlock(2, Nil, StackState(), Halt)
          )
        ))
      }
      "should fix up return addresses" in {
        val a = BasicBlock(0, Nil, StackState(List(ConstExpr(1))), ConditionalExit(ConstJump(1), FunctionReturn(1)))
        val b = BasicBlock(1, Nil, StackState(), FunctionReturn(2))

        val graph = ControlGraph(
          a, b
        )
        val rewritten = findIfRewrite(graph)
        rewritten should equal (Some(
          ControlGraph(
            IfBlock(0,
              a,
              b,
              StackState(List(ConstExpr(1))),
              FunctionReturn(1)
            )
          )
        ))
      }
    }
    "findUnlessRewrite" - {
      "should rewrite simple unless blocks" in {
        val graph = ControlGraph(
          BasicBlock(0, Nil, StackState(), ConditionalExit(ConstJump(2), ConstJump(1))),
          BasicBlock(1, Nil, StackState(), ConstJump(2)),
          BasicBlock(2, Nil, StackState(), Halt)
        )
        val rewritten = findUnlessRewrite(graph)
        rewritten should equal (Some(
          ControlGraph(
            UnlessBlock(0,
              BasicBlock(0, Nil, StackState(), ConditionalExit(ConstJump(2), ConstJump(1))),
              BasicBlock(1, Nil, StackState(), ConstJump(2)),
              StackState(),
              ConstJump(2)
            ),
            BasicBlock(2, Nil, StackState(), Halt)
          )
        ))
      }
    }
  }
}
