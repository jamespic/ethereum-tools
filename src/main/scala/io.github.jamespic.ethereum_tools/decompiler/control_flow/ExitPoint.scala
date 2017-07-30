package io.github.jamespic.ethereum_tools.decompiler.control_flow

sealed trait ExitPoint
sealed trait SingleReturn extends ExitPoint
sealed trait EarlyReturnWrapper extends ExitPoint {
  val wrapped: ExitPoint
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
  def canWrap(exitPoint: ExitPoint): Boolean = exitPoint match {
    case ReturnSafety(AnyReturn|FunctionReturnOnly(`depth`)) => true
    case _ => false
  }
  assert(canWrap(wrapped))
}

case class WithEarlyContractReturn(wrapped: ExitPoint) extends EarlyReturnWrapper {
  def canWrap(exitPoint: ExitPoint): Boolean = exitPoint match {
    case ReturnSafety(AnyReturn|ContractReturnOnly) => true
    case _ => false
  }
  assert(canWrap(wrapped))
}

object EarlyReturnWrapper {
  def unapply(wrapper: EarlyReturnWrapper) = Some(wrapper.wrapped)
}

object ExitPoint {
  def wrapEarlyContractReturn(e: ExitPoint) = e match {
    case WithEarlyContractReturn(_)|Halt => e
    case _ => WithEarlyContractReturn(e)
  }
  def wrapEarlyFunctionReturn(d: Int)(e: ExitPoint) = e match {
    case WithEarlyFunctionReturn(`d`, _)|FunctionReturn(`d`) => e
    case _ => WithEarlyFunctionReturn(d, e)
  }
}
