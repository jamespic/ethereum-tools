package io.github.jamespic.ethereum_tools.decompiler.control_flow

case class BlockEnd(exitPoint: ExitPoint = ConstJump(0), stackState: StackState = StackState()) {
  def =~(that: BlockEnd) = BlockEnd.merge(this, that).nonEmpty
  def merge(that: BlockEnd) = BlockEnd.merge(this, that)
  def &(that: BlockEnd) = BlockEnd.merge(this, that).get

  def =~>(that: BlockEnd) = BlockEnd.chain(this, that).nonEmpty
  def chain(that: BlockEnd) = BlockEnd.chain(this, that)
  def >>(that: BlockEnd) = BlockEnd.chain(this, that).get
}

object BlockEnd {
  def merge(a: BlockEnd, b: BlockEnd): Option[BlockEnd] = {
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
      for (exitPoint <- unifyExitPoints(a.exitPoint, b.exitPoint)) yield BlockEnd(exitPoint, stackState)
    }
  }

  private def unifyExitPoints(x: ExitPoint, y: ExitPoint): Option[ExitPoint] = (x, y) match {
    case (x , CalculatedJump) => None
    case (CalculatedJump, x) => None

    case (x, y) if x == y => Some(x)

    case (x , Throw) => Some(x)
    case (Throw, x) => Some(x)

    case (WithEarlyContractReturn(x), WithEarlyContractReturn(y)) =>
      unifyExitPoints(x, y) map ExitPoint.wrapEarlyContractReturn
    case (e @ WithEarlyContractReturn(x), y) if e.canWrap(y) =>
      unifyExitPoints(x, y) map ExitPoint.wrapEarlyContractReturn
    case (x, e @ WithEarlyContractReturn(y)) if e.canWrap(x) =>
      unifyExitPoints(x, y) map ExitPoint.wrapEarlyContractReturn

    case (WithEarlyFunctionReturn(d1, x), WithEarlyFunctionReturn(d2, y)) if d1 == d2 =>
      unifyExitPoints(x, y) map ExitPoint.wrapEarlyFunctionReturn(d1)
    case (e @ WithEarlyFunctionReturn(d, x), y) if e.canWrap(y) =>
      unifyExitPoints(x, y) map ExitPoint.wrapEarlyFunctionReturn(d)
    case (x, e @ WithEarlyFunctionReturn(d, y)) if e.canWrap(x) =>
      unifyExitPoints(x, y) map ExitPoint.wrapEarlyFunctionReturn(d)

    case (x @ ReturnSafety(ContractReturnOnly|AnyReturn), Halt) =>
      Some(ExitPoint.wrapEarlyContractReturn(x))
    case (Halt, x @ ReturnSafety(ContractReturnOnly|AnyReturn)) =>
      Some(ExitPoint.wrapEarlyContractReturn(x))

    case (x @ ReturnSafety(AnyReturn), FunctionReturn(d)) =>
      Some(ExitPoint.wrapEarlyFunctionReturn(d)(x))
    case (x @ ReturnSafety(FunctionReturnOnly(d1)), FunctionReturn(d2)) if d1 == d2 =>
      Some(ExitPoint.wrapEarlyFunctionReturn(d2)(x))
    case (FunctionReturn(d), x @ ReturnSafety(AnyReturn)) =>
      Some(ExitPoint.wrapEarlyFunctionReturn(d)(x))
    case (FunctionReturn(d1), x @ ReturnSafety(FunctionReturnOnly(d2))) if d1 == d2 =>
      Some(ExitPoint.wrapEarlyFunctionReturn(d1)(x))

    case _ => None
  }
  def chain(a: BlockEnd, b: BlockEnd): Option[BlockEnd] = {
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

    def fixUpReturnDepth(exitPoint: ExitPoint): Option[ExitPoint] = exitPoint match {
      case ConstJump(_)|CalculatedJump|Halt|Throw|WithEarlyContractReturn(_) => Some(exitPoint)
      case FunctionReturn(depth) =>
        aStack(depth) match {
          case ConstExpr(n) => Some(ConstJump(n.toInt))
          case StackVar(d) => Some(FunctionReturn(d))
          case CalculatedExpr => Some(CalculatedJump)
        }
      case WithEarlyFunctionReturn(depth, wrapped) =>
        for {
          fixedReturn <- fixUpReturnDepth(FunctionReturn(depth))
          fixedWrapped <- fixUpReturnDepth(wrapped)
          result <- unifyExitPoints(fixedReturn, fixedWrapped)
        } yield result
      case ConditionalExit(trueExit, falseExit) =>
        for {
          fixedTrue <- fixUpReturnDepth(trueExit)
          fixedFalse <- fixUpReturnDepth(falseExit)
          (checkedFixedTrue: SingleReturn, checkedFixedFalse: SingleReturn) = (fixedTrue, fixedFalse)
        } yield ConditionalExit(checkedFixedTrue, checkedFixedFalse)
    }

    def propagateReturnSafety(from: ExitPoint, to: ExitPoint) = (from, to) match {
      case (ReturnSafety(AnyReturn), _) => Some(to)
      case (ReturnSafety(ContractReturnOnly), ReturnSafety(AnyReturn|ContractReturnOnly)) =>
        Some(ExitPoint.wrapEarlyContractReturn(to))
      case (ReturnSafety(FunctionReturnOnly(depth)), ReturnSafety(AnyReturn)) =>
        Some(ExitPoint.wrapEarlyFunctionReturn(depth)(to))
      case (ReturnSafety(FunctionReturnOnly(depth)), ReturnSafety(FunctionReturnOnly(depth2))) if depth == depth2 =>
        Some(ExitPoint.wrapEarlyFunctionReturn(depth)(to))
      case _ => None
    }

    for {
      fixedUpExitPoint <- fixUpReturnDepth(b.exitPoint)
      exitPoint <- propagateReturnSafety(a.exitPoint, fixedUpExitPoint)
    } yield BlockEnd(exitPoint, stackState)
  }
}
