package io.github.jamespic.ethereum_tools.decompiler.control_flow

object GraphRewriteRules {
  def doSafeRewrites(graph: ControlGraph): ControlGraph = graph
  def tryUnsafeRewrites(graph: ControlGraph): Stream[ControlGraph] = Stream(graph)
  def rewrite(graph: ControlGraph): ControlGraph = graph
  def fullyRewritten(graph: ControlGraph) = graph.blocks forall {
    case BasicBlock(0, _, _, Halt) => true
    case _: FunctionBlock => true
    case _ => false
  }
}
