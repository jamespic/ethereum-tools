package io.github.jamespic.ethereum_tools.decompiler.control_flow

object Func {
  case class FuncEntry(address: Int, inputs: Int, outputs: Int)
  case class SignatureHint(callingBlockAddress: Int, inputs: Int, outputs: Int, returnAddress: Int)
  case class FuncInfo(knownFunctions: Set[FuncEntry], callSignatures: Set[SignatureHint])

  def identifyFunctionsByReturn(graph: ControlGraph): FuncInfo = {
    case class VisitedState(nextAddress: Int, blocksInStack: Set[Block])
    case class WalkResult(
      shouldUnwind: Boolean,
      visitedStates: Set[VisitedState],
      knownFunctionLocations: Set[FuncEntry] = Set.empty,
      unwindingFunctions: List[StateChange] = Nil,
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
        case Halt => WalkResult(true, visitedStates)
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
                val unwinds = for (unwindingFunction <- walkResult.unwindingFunctions) yield
                  (currentBlock.stateChange >> unwindingFunction, unwindingFunction)
                unwinds match {
                  case (StateChange(ConstJump(returnAddr), _), StateChange(StackJump(inputs), funcStack)) :: rest =>
                    // This function was a function call corresponding to a known return
                    val outputs = funcStack.height + inputs + 1
                    val func = FuncEntry(nextBlock.address, inputs, outputs)
                    val signatureHint = SignatureHint(currentBlock.address, inputs, outputs, returnAddr)
                    walkResult.copy(
                      knownFunctionLocations = walkResult.knownFunctionLocations + func,
                      unwindingFunctions = rest map (_._1),
                      signatureHints = walkResult.signatureHints + signatureHint
                    )
                  case unwinds =>
                    val unwindingFunctions = unwinds map (_._1)
                    // If current block wasn't a function call, then it might be a function return
                    currentBlock.exitPoint match {
                      case StackJump(_) =>
                        walkResult.copy(unwindingFunctions = currentBlock.stateChange :: unwindingFunctions)
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
}

case class Func(address: Int, code: ControlGraph, inputs: Int, outputs: Int)
