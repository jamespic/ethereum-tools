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
    case ConstJump(_)|FunctionReturn(`depth`)|CalculatedJump|Throw => true
    case FunctionReturn(_) => false
    case Halt => false
    case ConditionalExit(trueExit, falseExit) => canWrap(trueExit) && canWrap(falseExit)
    case _: EarlyReturnWrapper => false
  }
  assert(canWrap(wrapped))
}

case class WithEarlyContractReturn(wrapped: ExitPoint) extends EarlyReturnWrapper {
  def canWrap(exitPoint: ExitPoint): Boolean = exitPoint match {
    case ConstJump(_)|Halt|CalculatedJump|Throw => true
    case FunctionReturn(_) => false
    case ConditionalExit(trueExit, falseExit) => canWrap(trueExit) && canWrap(falseExit)
    case _: EarlyReturnWrapper => false
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

    case (x @ (ConstJump(_)|ConditionalExit(_, _)), Halt) =>
      Some(WithEarlyContractReturn(x))
    case (Halt, x @ (ConstJump(_)|ConditionalExit(_, _))) =>
      Some(WithEarlyContractReturn(x))

    case (x @ (ConstJump(_)|ConditionalExit(_, _)), FunctionReturn(d)) =>
      Some(WithEarlyFunctionReturn(d, x))
    case (FunctionReturn(d), x @ (ConstJump(_)|ConditionalExit(_, _))) =>
      Some(WithEarlyFunctionReturn(d, x))

    case _ => None
  }
  private def wrapEarlyContractReturn(e: ExitPoint) = e match {
    case WithEarlyContractReturn(inner) => e
    case _ => WithEarlyContractReturn(e)
  }
  private def wrapEarlyFunctionReturn(d: Int)(e: ExitPoint) = e match {
    case WithEarlyFunctionReturn(`d`, inner) => e
    case _ => WithEarlyFunctionReturn(d, e)
  }
}
