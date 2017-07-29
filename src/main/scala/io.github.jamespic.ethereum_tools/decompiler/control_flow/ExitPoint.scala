package io.github.jamespic.ethereum_tools.decompiler.control_flow

sealed trait ExitPoint

sealed trait SingleReturn extends ExitPoint
sealed trait EarlyReturnWrapper extends ExitPoint {
  val wrapped: ExitPoint
}
object EarlyReturnWrapper {
  def unapply(wrapper: EarlyReturnWrapper) = Some(wrapper.wrapped)
}

case class ConstJump(address: Int) extends SingleReturn {
  override def toString = f"ConstJump($address%04x)"
}
case class FunctionReturn(depth: Int) extends SingleReturn
case object CalculatedJump extends SingleReturn
case object Halt extends SingleReturn
case object Throw extends SingleReturn
case class ConditionalExit(trueExit: SingleReturn, falseExit: SingleReturn) extends ExitPoint



case class WithEarlyFunctionReturn(depth: Int, wrapped: ExitPoint) extends EarlyReturnWrapper {
  private def canWrap(exitPoint: ExitPoint): Boolean = exitPoint match {
    case ConstJump(_)|FunctionReturn(_)|CalculatedJump|Throw => true
    case Halt => false
    case ConditionalExit(trueExit, falseExit) => canWrap(trueExit) && canWrap(falseExit)
    case _: EarlyReturnWrapper => false
  }
  assert(canWrap(wrapped))
}

case class WithEarlyContractReturn(wrapped: ExitPoint) extends EarlyReturnWrapper {
  private def canWrap(exitPoint: ExitPoint): Boolean = exitPoint match {
    case ConstJump(_)|Halt|CalculatedJump|Throw => true
    case FunctionReturn(_) => false
    case ConditionalExit(trueExit, falseExit) => canWrap(trueExit) && canWrap(falseExit)
    case _: EarlyReturnWrapper => false
  }
  assert(canWrap(wrapped))
}
