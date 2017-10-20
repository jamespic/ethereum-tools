package io.github.jamespic.ethereum_tools.static_analysis.listeners

import io.github.jamespic.ethereum_tools.Bytecode._
import io.github.jamespic.ethereum_tools.static_analysis.Execution._
import io.github.jamespic.ethereum_tools.static_analysis._
import io.github.jamespic.ethereum_tools.static_analysis.StaticAnalysis.{Interesting, NotInteresting, StateListener}
import io.github.jamespic.ethereum_tools.static_analysis.constraints._

case class ReallySpecificListener(interested: Boolean = false) extends StateListener[Boolean] {
  val sig = Rational(0x2e1a7d4dl)
  val equalsSig = Range(ClosedBound(sig), ClosedBound(sig))
  override def apply(state: Execution.ExecutionState): StateListener[Boolean] = {
    val c = getContext(state).constraints.linearConstraints.constraints
    if (
      (c.get(LinearClause[AttackerControlled](CallData(0, 4, 2) -> Rational(1))) contains equalsSig)
      && (c.get(LinearClause[AttackerControlled](CallData(0, 4, 4) -> Rational(1))) contains equalsSig)
      && (c.get(LinearClause[AttackerControlled](
        CallData(4, 32, 2) -> Rational(-1),
        SpentMoney(0) -> Rational(1)
      )) exists (_ != Range(NoBound, ClosedBound(0))))
      && (c.get(LinearClause[AttackerControlled](
        CallData(4, 32, 4) -> Rational(-1),
        SpentMoney(0) -> Rational(1)
      )) exists (_ != Range(NoBound, ClosedBound(0))))
      && (
        state match {
          case x: FinishedState => true
          case x: RunningState => x.nextInst == STOP || x.nextInst == RETURN
          case _ => false
        }

      )
    ) {
      ReallySpecificListener(true)
    } else {
      ReallySpecificListener(false)
    }
  }

  def getContext(state: ExecutionState): Execution.Context = state match {
    case FinishedState(context, _, _, _) => context
    case AttackerContractState(calledState, _, _, _, _) => getContext(calledState)
    case ContractCallState(_, calledState, _, _, _) => getContext(calledState)
    case x: RunningState => x.context
  }

  override def startNewTransaction(state: Execution.ExecutionState): StateListener[Boolean] = this

  override def interest: StaticAnalysis.Interest[Boolean] = if (interested) Interesting(true) else NotInteresting
}
