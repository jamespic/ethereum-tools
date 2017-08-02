package io.github.jamespic.ethereum_tools.decompiler.control_flow

sealed trait ExitPoint

case class ConstJump(address: Int) extends ExitPoint {
  override def toString = f"ConstJump($address%04x)"
}
case class StackJump(depth: Int) extends ExitPoint
case object CalculatedJump extends ExitPoint
case object Halt extends ExitPoint
case class FunctionReturn(outputs: Int) extends ExitPoint
case object Throw extends ExitPoint
case class ConditionalExit(trueExit: ExitPoint, falseExit: ExitPoint) extends ExitPoint
