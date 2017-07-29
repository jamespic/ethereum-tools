package io.github.jamespic.ethereum_tools.decompiler.control_flow

object GraphRewriteRules {
  def doSafeRewrites(graph: ControlGraph): ControlGraph = graph
  def tryUnsafeRewrites(graph: ControlGraph): Stream[ControlGraph] = Stream(graph)
  def rewrite(graph: ControlGraph): ControlGraph = graph
  def fullyRewritten(graph: ControlGraph, entryPoint: Int = 0) = graph.blocks forall {
    case BasicBlock(`entryPoint`, _, _, Halt) => true
    case _: FunctionBlock => true
    case _ => false
  }
}
