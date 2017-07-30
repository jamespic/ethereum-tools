package io.github.jamespic.ethereum_tools.decompiler.control_flow

object ReturnSafety {
  def unapply(exitPoint: ExitPoint): Option[ReturnSafety] = exitPoint match {
    case ConstJump(_)
         |Throw => Some(AnyReturn)
    case ConditionalExit(
      ReturnSafety(AnyReturn),
      ReturnSafety(AnyReturn)
    ) => Some(AnyReturn)
    case Halt
         |WithEarlyContractReturn(_)
         |ConditionalExit(
            ReturnSafety(AnyReturn|ContractReturnOnly),
            ReturnSafety(AnyReturn|ContractReturnOnly)
          ) => Some(ContractReturnOnly)
    case ConditionalExit(
      ReturnSafety(AnyReturn),
      ReturnSafety(FunctionReturnOnly(n))
    ) => Some(FunctionReturnOnly(n))
    case ConditionalExit(
      ReturnSafety(FunctionReturnOnly(n)),
      ReturnSafety(AnyReturn)
    ) => Some(FunctionReturnOnly(n))
    case ConditionalExit(
      ReturnSafety(FunctionReturnOnly(n)),
      ReturnSafety(FunctionReturnOnly(m))
    ) if m == n => Some(FunctionReturnOnly(n))
    case FunctionReturn(n) => Some(FunctionReturnOnly(n))
    case WithEarlyFunctionReturn(n, _) => Some(FunctionReturnOnly(n))
    case _ => None  // CalculatedJump, and any unsafe ConditionalExit combinations
  }
  def apply(exitPoint: ExitPoint) = unapply(exitPoint)
}

sealed trait ReturnSafety
case object ContractReturnOnly extends ReturnSafety
case object AnyReturn extends ReturnSafety
case class FunctionReturnOnly(depth: Int) extends ReturnSafety
