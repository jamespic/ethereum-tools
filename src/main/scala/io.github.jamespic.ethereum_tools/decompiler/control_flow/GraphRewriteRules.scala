package io.github.jamespic.ethereum_tools.decompiler.control_flow

object GraphRewriteRules {
  def doSafeRewrites(graph: ControlGraph): ControlGraph = {
    findSafeRewrite(graph) match {
      case Some(x) => doSafeRewrites(x)
      case None => graph
    }
  }
  def tryUnsafeRewrites(graph: ControlGraph): Stream[ControlGraph] = Stream()
  def rewrite(graph: ControlGraph): ControlGraph = doSafeRewrites(graph)

  def findSafeRewrite(graph: ControlGraph): Option[ControlGraph] = (
    stripUnreachable(graph)
    orElse findIfRewrite(graph)
    orElse findUnlessRewrite(graph)
  )



  def stripUnreachable(graph: ControlGraph): Option[ControlGraph] = {
    if (graph.blocks.exists(_.exitPoint == CalculatedJump)) None
    else {
      val hasDanglingFunctionReturns = graph.blocks.exists {
        case Block(_, StateChange((StackJump(_)), _)) => true
        case _ => false
      }
      val reachableBlocks = (
        (graph.blocks flatMap (block => graph.exitBlocks(block.exitPoint)))
        ++ (
          // Be conservative and treat any const on a stack as a function return dest
          if (hasDanglingFunctionReturns) findJumpableBlocks(graph)
          else Set()
        ) + graph.blockByAddress(0))
      if (reachableBlocks != graph.blocks) Some(ControlGraph(reachableBlocks))
      else None
    }
  }

  private def findJumpableBlocks(graph: ControlGraph) = for {
    block <- graph.blocks
    ConstExpr(n) <- block.stackChange.vars
    reachedBlock <- graph.blockByAddress.get(n.toInt)
  } yield reachedBlock

  def findIfRewrite(graph: ControlGraph): Option[ControlGraph] = {
    graph.blocks collectFirst {
      // Simple If block
      case a @ Block(
        newAddress @ address,
        StateChange(ConditionalExit(
          graph.ExitBlock(b @ Block(_, endB)),
          exitA
        ), stackA)

      ) if (StateChange(exitA, stackA) =~ (StateChange(exitA, stackA) >> endB)) =>
        val newBlock = IfBlock(newAddress, a, b, StateChange(exitA, stackA) & StateChange(exitA, stackA) >> endB)
        ControlGraph(graph.blocks - a - b + newBlock)
    }
  }

  def findUnlessRewrite(graph: ControlGraph): Option[ControlGraph] = {
    graph.blocks collectFirst {
      // Simple If block
      case a @ Block(
        newAddress @ address,
        StateChange(ConditionalExit(
          exitA,
          graph.ExitBlock(b @ Block(_, endB))
        ), stackA)

      ) if (StateChange(exitA, stackA) =~ endB) =>
        val newBlock = UnlessBlock(newAddress, a, b, StateChange(exitA, stackA) & StateChange(exitA, stackA) >> endB)
        ControlGraph(graph.blocks - a - b + newBlock)
    }
  }


  def findPassthroughRewrite(graph: ControlGraph): Option[ControlGraph] = {
    val graphStream = for {
      a @ Block(
        addressA,
        endA @ graph.ExitBlock(
          b @ Block(addressB, endB)
        )
      ) <- graph.blocks.toStream
      if graph.parents(b.address) == Set(a)
      stateChange = endA >> endB
      newBlock: Block = PassThroughBlock(addressA, a, b, stateChange)
    } yield ControlGraph(graph.blocks - a - b + newBlock)
    graphStream.headOption
  }
}
