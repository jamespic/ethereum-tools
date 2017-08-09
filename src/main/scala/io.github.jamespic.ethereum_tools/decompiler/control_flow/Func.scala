package io.github.jamespic.ethereum_tools.decompiler.control_flow

object Func {
  case class FuncEntry(address: Int, inputs: Int, outputs: Int) {
    override def toString = f"FuncEntry($address%x, $inputs%d, $outputs%d"
  }
  case class SignatureHint(callingBlockAddress: Int, inputs: Int, outputs: Int, returnAddress: Int) {
    override def toString = f"SignatureHint($callingBlockAddress%x, $inputs%d, $outputs%d, $returnAddress%x)"
  }
  case class FuncInfo(knownFunctions: Set[FuncEntry], callSignatures: Set[SignatureHint])

  def identifyFunctionsByReturn(graph: ControlGraph): FuncInfo = {
    case class VisitedState(nextAddress: Int, blocksInStack: Set[Block])
    case class WalkResult(
      shouldUnwind: Boolean,
      visitedStates: Set[VisitedState],
      knownFunctionLocations: Set[FuncEntry] = Set.empty,
      unwindingFunctions: Set[StateChange] = Set.empty,
      signatureHints: Set[SignatureHint] = Set.empty
    ) {
      def +(that: WalkResult) = WalkResult(
        this.shouldUnwind || that.shouldUnwind,
        this.visitedStates ++ that.visitedStates,
        this.knownFunctionLocations ++ that.knownFunctionLocations,
        this.unwindingFunctions ++ that.unwindingFunctions,
        this.signatureHints ++ that.signatureHints
      )
    }
    def walk(
        currentBlock: Block,
        stateChange: StateChange = StateChange(),
        blocksInStack: Set[Block] = Set.empty,
        visitedStates: Set[VisitedState] = Set.empty): WalkResult = {
      currentBlock.exitPoint match {
        case Halt =>
          WalkResult(true, visitedStates)
        case ep =>
          val newBlocksInStack = blocksInStack + currentBlock
          val newStateChange = stateChange >> currentBlock.stateChange

          graph.exitBlocks(newStateChange.exitPoint).foldLeft(WalkResult(false, visitedStates)){(prevResult, nextBlock) =>
            val nextVisitedState = VisitedState(nextBlock.address, newBlocksInStack)
            if (prevResult.visitedStates contains nextVisitedState) prevResult
            else {
              val walkResult = walk(
                nextBlock,
                newStateChange,
                newBlocksInStack,
                prevResult.visitedStates + nextVisitedState
              )

              val unwoundResult = if (walkResult.shouldUnwind) {
                val unwindsPartitioned = for (unwindingFunction <- walkResult.unwindingFunctions) yield {
                  (currentBlock.stateChange >> unwindingFunction) match {
                    case StateChange(ConstJump(returnAddr), _) =>
                      val StateChange(StackJump(inputs), funcStack) = unwindingFunction
                      val outputs = funcStack.height + inputs + 1
                      Left(SignatureHint(currentBlock.address, inputs, outputs, returnAddr))
                    case unwound => Right(unwound)
                  }
                }
                val signatureHints = for (Left(signatureHint) <- unwindsPartitioned) yield signatureHint
                val unwindingFunctions = for (Right(unwound) <- unwindsPartitioned) yield unwound
                val knownFunctions = for (SignatureHint(_, inputs, outputs, _) <- signatureHints) yield
                  FuncEntry(nextBlock.address, inputs, outputs)

                if (knownFunctions.nonEmpty) {
                  // This function was a function call corresponding to a known return
                  walkResult.copy(
                    knownFunctionLocations = walkResult.knownFunctionLocations ++ knownFunctions,
                    unwindingFunctions = unwindingFunctions,
                    signatureHints = walkResult.signatureHints ++ signatureHints
                  )
                } else {
                  // If current block wasn't a function call, then it might be a function return
                  currentBlock.exitPoint match {
                    case StackJump(_) =>
                      walkResult.copy(unwindingFunctions = unwindingFunctions + currentBlock.stateChange )
                    case _ =>
                      walkResult.copy(unwindingFunctions = unwindingFunctions)
                  }
                }
              } else walkResult
              prevResult + unwoundResult
            }
          }
      }
    }
    val WalkResult(shouldUnwind, _, knownFunctionLocations, unwindingFunctions, signatureHints) = walk(graph.blockByAddress(0))
    assert(shouldUnwind, "This contract doesn't seem to halt - this may indicate a problem with decompilation")
    assert(unwindingFunctions.isEmpty, "Not all function returns have been accounted for")
    FuncInfo(knownFunctionLocations, signatureHints)
  }

  //FIXME: Identify non-halting functions by shared jumps
}

case class Func(address: Int, code: ControlGraph, inputs: Int, outputs: Int)
