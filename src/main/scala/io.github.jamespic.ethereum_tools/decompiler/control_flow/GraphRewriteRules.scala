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
  def fullyRewritten(graph: ControlGraph, entryPoint: Int = 0) = graph.blocks forall {
    case BasicBlock(`entryPoint`, _, _, Halt) => true
    case _: FunctionBlock => true
    case _ => false
  }

  def findSafeRewrite(graph: ControlGraph): Option[ControlGraph] = (
    stripUnreachable(graph)
    orElse findIfRewrite(graph)
    orElse findUnlessRewrite(graph)
  )

  def stripUnreachable(graph: ControlGraph): Option[ControlGraph] = {
    if (graph.blocks.exists(_.exitPoint == CalculatedJump)) None
    else {
      val hasDanglingFunctionReturns = graph.blocks.exists {
        case Block(_, FunctionReturn(_)|WithEarlyFunctionReturn(_, _)) => true
        case _ => false
      }
      val reachableBlocks = (
        (graph.blocks flatMap (block => graph.exitBlocks(block.exitPoint)))
        ++ (
          if (hasDanglingFunctionReturns) {
            // Be conservative and treat any const on a stack as a jump dest
            for {
              block <- graph.blocks
              ConstExpr(n) <- block.stackChange.vars
              reachedBlock <- graph.blockByAddress.get(n.toInt)
            } yield reachedBlock
          } else Set()
        ) + graph.blockByAddress(0))
      if (reachableBlocks != graph.blocks) Some(ControlGraph(reachableBlocks))
      else None
    }
  }

  def findIfRewrite(graph: ControlGraph): Option[ControlGraph] = {
    graph.blocks collectFirst {
      // Simple If block
      case a @ Block(newAddress @ address,
        ConditionalExit(
          graph.ExitBlock(b @ Block(adress, exitB)),
          exitA
        )
      ) if (exitA =~ exitB) && (a.stackChange =~ a.stackChange >> b.stackChange) =>
        val newBlock = IfBlock(newAddress, a, b, a.stackChange & a.stackChange >> b.stackChange, exitA & exitB)
        ControlGraph(graph.blocks - a - b + newBlock)
    }
  }

  def findUnlessRewrite(graph: ControlGraph): Option[ControlGraph] = {
    graph.blocks collectFirst {
      // Simple Unless block
      case a @ Block(newAddress @ address,
        ConditionalExit(
          exitA,
          graph.ExitBlock(b @ Block(adress, exitB))
        )
      ) if (exitA =~ exitB) && (a.stackChange =~ a.stackChange >> b.stackChange) =>
        val newBlock = UnlessBlock(newAddress, a, b, a.stackChange & a.stackChange >> b.stackChange, exitA & exitB)
        ControlGraph(graph.blocks - a - b + newBlock)
    }
  }

  def findPassthroughRewrite(graph: ControlGraph): Option[ControlGraph] = {
    val graphStream = for {
      a @ Block(
        addressA,
        exitA @ graph.ExitBlock(
          b @ Block(addressB, exitB)
        )
      ) <- graph.blocks.toStream
      if graph.parents(b.address) == Set(a)
      Some(fixedExitB) = ExitPoint.fixUpReturnDepth(exitB, a.stackChange)
      Some(exitPoint) = (exitA, exitB) match {
        case (ReturnSafety(AnyReturn), _) => Some(exitB)
        case (ReturnSafety(ContractReturnOnly), ReturnSafety(AnyReturn|ContractReturnOnly)) =>
          Some(ExitPoint.wrapEarlyContractReturn(exitB))
        case (ReturnSafety(FunctionReturnOnly(depth)), ReturnSafety(AnyReturn)) =>
          Some(ExitPoint.wrapEarlyFunctionReturn(depth)(exitB))
        case (ReturnSafety(FunctionReturnOnly(depth)), ReturnSafety(FunctionReturnOnly(depth2))) if depth == depth2 =>
          Some(ExitPoint.wrapEarlyFunctionReturn(depth)(exitB))
        case _ => None
      }
      newBlock: Block = PassThroughBlock(addressA, a, b, a.stackChange >> b.stackChange, exitPoint)
    } yield ControlGraph(graph.blocks - a - b + newBlock)
    graphStream.headOption
  }
}
