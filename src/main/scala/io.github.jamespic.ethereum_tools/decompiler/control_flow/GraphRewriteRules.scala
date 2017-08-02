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
    case BasicBlock(`entryPoint`, _, BlockEnd(Halt, _)) => true
    case _: FunctionBlock => true
    case _ => false
  }

  def findSafeRewrite(graph: ControlGraph): Option[ControlGraph] = (
    stripUnreachable(graph)
    orElse findIfRewrite(graph)
    orElse findUnlessRewrite(graph)
  )

  def identifyFunctions(graph: ControlGraph) = {
    /*
     * Distinguishing between function returns and function variable calls is
     * tricky. However, observe that last one before a contract return must be
     * a function return. So we walk the control graph, keeping track of the
     * stack. When we reach a Halt, we unwind the stack until we find a StackJump,
     * which must be a function return, then work back until we find the
     * block that added its return value onto the stack.
     */
    case class VisitedState(nextAddress: Int, blocksInStack: Set[Block])
    case class FunctionEntry(address: Int, inputs: Int, outputs: Int)
    case class SignatureHint(inputs: Int, outputs: Int)
    case class WalkResult(
      shouldUnwind: Boolean,
      visitedStates: Set[VisitedState],
      knownFunctionLocations: Set[FunctionEntry] = Set.empty,
      unwindingFunctions: Set[BlockEnd] = Set.empty,
      signatureHints: Map[Block, SignatureHint] = Map.empty
    ) {
      def +(that: WalkResult) = WalkResult(
        this.shouldUnwind || that.shouldUnwind,
        this.visitedStates ++ that.visitedStates,
        this.knownFunctionLocations ++ that.knownFunctionLocations,
        this.unwindingFunctions ++ that.unwindingFunctions,
        this.signatureHints ++ that.signatureHints
      )

      def unwind(unwindBlock: Block): WalkResult = {
        if (shouldUnwind) {
          // Figure out if this is a function call by seeing if this is the block that
          // provided the return address for any unwinding functions
          val unwinds = for (unwindingFunction <- unwindingFunctions) yield
            (unwindBlock.blockEnd >> unwindingFunction, unwindingFunction)
          val (fullyUnwound, notFullyUnwound) = unwinds partition {
            case (BlockEnd(ConstJump(_), _), _) => true
            case (BlockEnd(StackJump(_), _), _) => false
          }
          assert(fullyUnwound.size <= 1)
          if (fullyUnwound.nonEmpty) {
            // currentBlock is a function call
            val (
              BlockEnd(ConstJump(functionAddress), _),
              BlockEnd(StackJump(depth), functionStack)
            ) = fullyUnwound.head
            val inputs = depth
            val outputs = functionStack.height
            copy(
              knownFunctionLocations =
                knownFunctionLocations
                + FunctionEntry(functionAddress, inputs, outputs),
              signatureHints =
                signatureHints + (unwindBlock -> SignatureHint(inputs, outputs)),
              unwindingFunctions =
                notFullyUnwound map (_._1)
            )
          } else unwindBlock.blockEnd.exitPoint match {
            // If it's a stack jump, it must be a function return (otherwise it wouldhave matched above)
            case StackJump(_) => copy(unwindingFunctions = unwindingFunctions + unwindBlock.blockEnd)
            case _ => this
          }
        } else {
          this
        }
      }
    }

    case class WalkState(
      blockEnd: BlockEnd = BlockEnd(),
      jumpStack: List[Block] = Nil,
      visitedStates: Set[VisitedState] = Set.empty,
      knownFunctionLocations: Set[FunctionEntry] = Set.empty
    ) {
      def innerWalk(): WalkResult = blockEnd.exitPoint match {
        case ConstJump(address) =>
          if (visitedStates contains VisitedState(address, jumpStack.toSet)) WalkResult(
            shouldUnwind = false, visitedStates = visitedStates
          )
          else {
            val nextBlock = graph.blockByAddress(address)
            val newJumpStack = nextBlock :: jumpStack
            copy(
              blockEnd = blockEnd >> nextBlock.blockEnd,
              jumpStack = newJumpStack,
              visitedStates = visitedStates + VisitedState(address, newJumpStack.toSet)
            ).walk()
          }
        case CalculatedJump|Throw => // Can't tell us anything
          WalkResult(
            shouldUnwind = true,
            visitedStates = visitedStates
          )
        case StackJump(_) => throw new AssertionError("Stack jump must have known jump location")
        case FunctionReturn(_) => throw new AssertionError("Function returns should not have been calculated yet")
        case Halt =>
          WalkResult(
            shouldUnwind = true,
            visitedStates = visitedStates
          )
        case ConditionalExit(trueExit, falseExit) =>
          val trueResult = copy(
            blockEnd = blockEnd.copy(exitPoint = trueExit)
          ).innerWalk()
          val falseResult = copy(
            visitedStates = visitedStates ++ trueResult.visitedStates,
            knownFunctionLocations = knownFunctionLocations ++ trueResult.knownFunctionLocations,
            blockEnd = blockEnd.copy(exitPoint = falseExit)
          ).innerWalk()
          trueResult + falseResult
      }

      def walk(): WalkResult = {
        // Walk the call graph
        val walkResult: WalkResult = innerWalk()

        jumpStack match {
          case head :: tail => walkResult.unwind(head)
          case Nil => walkResult
        }
      }
    }
    WalkState().walk()
    // val WalkResult(_, _, functionLocations, _, signatureHints) = WalkState().walk()
    // // Now break it into a series of disjoint function blocks, and if any of the
    // // blocks prove not to be disjoint, see if they make sense as functions
    // // in their own right
    // ???
  }

  def stripUnreachable(graph: ControlGraph): Option[ControlGraph] = {
    if (graph.blocks.exists(_.exitPoint == CalculatedJump)) None
    else {
      val hasDanglingFunctionReturns = graph.blocks.exists {
        case Block(_, BlockEnd((StackJump(_)), _)) => true
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
        BlockEnd(ConditionalExit(
          graph.ExitBlock(b @ Block(_, endB)),
          exitA
        ), stackA)

      ) if (BlockEnd(exitA, stackA) =~ (BlockEnd(exitA, stackA) >> endB)) =>
        val newBlock = IfBlock(newAddress, a, b, BlockEnd(exitA, stackA) & BlockEnd(exitA, stackA) >> endB)
        ControlGraph(graph.blocks - a - b + newBlock)
    }
  }

  def findUnlessRewrite(graph: ControlGraph): Option[ControlGraph] = {
    graph.blocks collectFirst {
      // Simple If block
      case a @ Block(
        newAddress @ address,
        BlockEnd(ConditionalExit(
          exitA,
          graph.ExitBlock(b @ Block(_, endB))
        ), stackA)

      ) if (BlockEnd(exitA, stackA) =~ endB) =>
        val newBlock = UnlessBlock(newAddress, a, b, BlockEnd(exitA, stackA) & BlockEnd(exitA, stackA) >> endB)
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
      blockEnd = endA >> endB
      newBlock: Block = PassThroughBlock(addressA, a, b, blockEnd)
    } yield ControlGraph(graph.blocks - a - b + newBlock)
    graphStream.headOption
  }
}
