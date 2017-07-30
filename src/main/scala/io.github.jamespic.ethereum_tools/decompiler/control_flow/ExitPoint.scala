package io.github.jamespic.ethereum_tools.decompiler.control_flow

sealed trait ExitPoint {
  def =~(that: ExitPoint) = ExitPoint.unify(this, that).nonEmpty
  def &(that: ExitPoint) = ExitPoint.unify(this, that).get
}

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
  def unify(x: ExitPoint, y: ExitPoint): Option[ExitPoint] = (x, y) match {
    case (x , CalculatedJump) => None
    case (CalculatedJump, x) => None

    case (x, y) if x == y => Some(x)

    case (x , Throw) => Some(x)
    case (Throw, x) => Some(x)

    case (WithEarlyContractReturn(x), WithEarlyContractReturn(y)) =>
      unify(x, y) map wrapEarlyContractReturn
    case (e @ WithEarlyContractReturn(x), y) if e.canWrap(y) =>
      unify(x, y) map wrapEarlyContractReturn
    case (x, e @ WithEarlyContractReturn(y)) if e.canWrap(x) =>
      unify(x, y) map wrapEarlyContractReturn

    case (WithEarlyFunctionReturn(d1, x), WithEarlyFunctionReturn(d2, y)) if d1 == d2 =>
      unify(x, y) map wrapEarlyFunctionReturn(d1)
    case (e @ WithEarlyFunctionReturn(d, x), y) if e.canWrap(y) =>
      unify(x, y) map wrapEarlyFunctionReturn(d)
    case (x, e @ WithEarlyFunctionReturn(d, y)) if e.canWrap(x) =>
      unify(x, y) map wrapEarlyFunctionReturn(d)

    case (x @ ReturnSafety(ContractReturnOnly|AnyReturn), Halt) =>
      Some(WithEarlyContractReturn(x))
    case (Halt, x @ ReturnSafety(ContractReturnOnly|AnyReturn)) =>
      Some(WithEarlyContractReturn(x))

    case (x @ ReturnSafety(AnyReturn), FunctionReturn(d)) =>
      Some(WithEarlyFunctionReturn(d, x))
    case (x @ ReturnSafety(FunctionReturnOnly(d1)), FunctionReturn(d2)) if d1 == d2 =>
      Some(WithEarlyFunctionReturn(d2, x))
    case (FunctionReturn(d), x @ ReturnSafety(AnyReturn)) =>
      Some(WithEarlyFunctionReturn(d, x))
    case (FunctionReturn(d1), x @ ReturnSafety(FunctionReturnOnly(d2))) if d1 == d2 =>
      Some(WithEarlyFunctionReturn(d1, x))

    case _ => None
  }
  def wrapEarlyContractReturn(e: ExitPoint) = e match {
    case WithEarlyContractReturn(_)|Halt => e
    case _ => WithEarlyContractReturn(e)
  }
  def wrapEarlyFunctionReturn(d: Int)(e: ExitPoint) = e match {
    case WithEarlyFunctionReturn(`d`, _)|FunctionReturn(`d`) => e
    case _ => WithEarlyFunctionReturn(d, e)
  }

  def fixUpReturnDepth(exitPoint: ExitPoint, stack: StackState): Option[ExitPoint] = exitPoint match {
    case ConstJump(_)|CalculatedJump|Halt|Throw|WithEarlyContractReturn(_) => Some(exitPoint)
    case FunctionReturn(depth) =>
      stack(depth) match {
        case ConstExpr(n) => Some(ConstJump(n.toInt))
        case StackVar(d) => Some(FunctionReturn(d))
        case CalculatedExpr => Some(CalculatedJump)
      }
    case WithEarlyFunctionReturn(depth, wrapped) =>
      for {
        fixedReturn <- fixUpReturnDepth(FunctionReturn(depth), stack)
        fixedWrapped <- fixUpReturnDepth(wrapped, stack)
        result <- unify(fixedReturn, fixedWrapped)
      } yield result
    case ConditionalExit(trueExit, falseExit) =>
      for {
        fixedTrue <- fixUpReturnDepth(trueExit, stack)
        fixedFalse <- fixUpReturnDepth(falseExit, stack)
        (checkedFixedTrue: SingleReturn, checkedFixedFalse: SingleReturn) = (fixedTrue, fixedFalse)
      } yield ConditionalExit(checkedFixedTrue, checkedFixedFalse)
  }
}

object ReturnSafety {
  def unapply(exitPoint: ExitPoint): Option[ReturnSafety] = exitPoint match {
    case ConstJump(_)|Throw => Some(AnyReturn)
    case ConditionalExit(
      ReturnSafety(AnyReturn),
      ReturnSafety(AnyReturn)
    ) => Some(AnyReturn)
    case Halt|ConditionalExit(
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
    case _ => None  // CalculatedJump, and any unsafe ConditionalExit combinations
  }
}

sealed trait ReturnSafety
case object ContractReturnOnly extends ReturnSafety
case object AnyReturn extends ReturnSafety
case class FunctionReturnOnly(depth: Int) extends ReturnSafety
