package io.github.jamespic.ethereum_tools.decompiler.control_flow

sealed trait ExitPoint
sealed trait SingleReturn extends ExitPoint
case class ConstJump(address: Int) extends SingleReturn {
  override def toString = f"ConstJump($address%04x)"
}
case class FunctionReturn(depth: Int) extends SingleReturn
case object CalculatedJump extends SingleReturn
case object Halt extends SingleReturn
case object Throw extends SingleReturn
case class ConditionalExit(trueExit: SingleReturn, falseExit: SingleReturn) extends ExitPoint
sealed trait EarlyReturnWrapper extends ExitPoint
case class WithEarlyFunctionReturn(depth: Int, exitPoint: ExitPoint) extends EarlyReturnWrapper {
  assert(!exitPoint.isInstanceOf[EarlyReturnWrapper])
}
case class WithEarlyContractReturn(exitPoint: ExitPoint) extends EarlyReturnWrapper {
  assert(!exitPoint.isInstanceOf[EarlyReturnWrapper])
}
