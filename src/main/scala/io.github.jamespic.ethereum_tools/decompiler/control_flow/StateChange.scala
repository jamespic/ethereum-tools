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
    if (a.stackState.height != b.stackState.height) None
    else {
      val stackState = {
        val minDepth = Math.max(a.stackState.vars.length, b.stackState.vars.length)
        val aStack = a.stackState.ensureDepth(minDepth)
        val bStack = b.stackState.ensureDepth(minDepth)
        assert(aStack.thenIndex == bStack.thenIndex)
        val newVars = for ((x, y) <- aStack.vars zip bStack.vars) yield {
          if (x == y) x else CalculatedExpr
        }
        StackState(newVars, aStack.thenIndex)
      }
      for (exitPoint <- unifyExitPoints(a.exitPoint, b.exitPoint)) yield StateChange(exitPoint, stackState)
    }
  }

  private def unifyExitPoints(x: ExitPoint, y: ExitPoint): Option[ExitPoint] = (x, y) match {
    case (x , CalculatedJump) => None
    case (CalculatedJump, x) => None

    case (x, y) if x == y => Some(x)

    case (x , Throw) => Some(x)
    case (Throw, x) => Some(x)

    case (Halt, FunctionReturn(_)) => None
    case (FunctionReturn(_), Halt) => None

    case (x , Halt|FunctionReturn(_)) => Some(x)
    case (Halt|FunctionReturn(_), x) => Some(x)

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
      case ConstJump(_)|CalculatedJump|Halt|Throw => exitPoint
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
