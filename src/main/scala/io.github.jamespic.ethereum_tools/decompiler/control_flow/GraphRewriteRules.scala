package io.github.jamespic.ethereum_tools.decompiler.control_flow

object GraphRewriteRules {
  def doSafeRewrites(graph: ControlGraph): ControlGraph = {
    findSafeRewrite(graph) match {
      case Some(x) => doSafeRewrites(x)
      case None => graph
    }
  }
  def tryUnsafeRewrites(graph: ControlGraph): Stream[ControlGraph] = Stream(graph)
  def rewrite(graph: ControlGraph): ControlGraph = doSafeRewrites(graph)
  def fullyRewritten(graph: ControlGraph, entryPoint: Int = 0) = graph.blocks forall {
    case BasicBlock(`entryPoint`, _, _, Halt) => true
    case _: FunctionBlock => true
    case _ => false
  }

  def findSafeRewrite(graph: ControlGraph): Option[ControlGraph] = {
    graph.blocks collectFirst {
      // Simple If block
      case a @ Block(newAddress @ address,
        ConditionalExit(
          graph.ExitBlock(b @ Block(adress, exitB)),
          exitA)
      ) if (exitA =~ exitB) && (a.stackChange =~ a.stackChange >> b.stackChange) =>
        val newBlock = IfBlock(newAddress, a, b, a.stackChange & a.stackChange >> b.stackChange, exitA & exitB)
        ControlGraph(graph.blocks - a - b + newBlock)
    }
  }
}
