package io.github.jamespic.ethereum_tools.decompiler.control_flow

case class StateChange(exitPoint: ExitPoint = ConstJump(0), stackState: StackState = StackState()) {
  def =~(that: StateChange) = StateChange.merge(this, that).nonEmpty
  def merge(that: StateChange) = StateChange.merge(this, that)
  def &(that: StateChange) = StateChange.merge(this, that).get

  def chain(that: StateChange) = StateChange.chain(this, that)
  def >>(that: StateChange) = StateChange.chain(this, that)
}

object StateChange {
  def merge(a: StateChange, b: StateChange): Option[StateChange] = {
    for {
      stackState <- StackState.merge(a.stackState, b.stackState)
      exitPoint <- unifyExitPoints(a.exitPoint, b.exitPoint)
    } yield StateChange(exitPoint, stackState)
  }

  private def unifyExitPoints(x: ExitPoint, y: ExitPoint): Option[ExitPoint] = (x, y) match {
    case (x , CalculatedJump) => None
    case (CalculatedJump, x) => None

    case (x, y) if x == y => Some(x)

    case (x , Throw) => Some(x)
    case (Throw, x) => Some(x)

    case (x , Halt) => Some(x)
    case (Halt, x) => Some(x)

    case _ => None
  }
  def chain(a: StateChange, b: StateChange): StateChange = {
    // Ensure new stack is deep enough
    val minDepth = a.stackState.vars.length + b.stackState.vars.length - b.stackState.thenIndex
    val aStack = a.stackState
    val bStack = b.stackState.ensureDepth(minDepth)
    val stackState = StackState(
      bStack.vars map {
        case StackVar(n) => aStack(n)
        case x => x
      },
      aStack.thenIndex + bStack.thenIndex - aStack.vars.length
    )

    def fixUpReturnDepth(exitPoint: ExitPoint): ExitPoint = exitPoint match {
      case ConstJump(_)|Fallthrough(_)|CalculatedJump|Halt|Throw => exitPoint
      case StackJump(depth) =>
        aStack(depth) match {
          case ConstExpr(n) => ConstJump(n.toInt)
          case StackVar(d) => StackJump(d)
          case CalculatedExpr => CalculatedJump
        }
      case ConditionalExit(trueExit, falseExit) =>
        val fixedTrue = fixUpReturnDepth(trueExit)
        val fixedFalse = fixUpReturnDepth(falseExit)
        ConditionalExit(fixedTrue, fixedFalse)
    }

    val fixedUpExitPoint = fixUpReturnDepth(b.exitPoint)
    StateChange(fixedUpExitPoint, stackState)
  }
}
