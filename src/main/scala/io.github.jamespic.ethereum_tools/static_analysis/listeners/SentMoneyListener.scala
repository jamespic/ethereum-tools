package io.github.jamespic.ethereum_tools.static_analysis.listeners

import io.github.jamespic.ethereum_tools.static_analysis._
import StaticAnalysis._
import io.github.jamespic.ethereum_tools.Bytecode._
import io.github.jamespic.ethereum_tools.static_analysis.Execution._
import io.github.jamespic.ethereum_tools.static_analysis.constraints._

case class SentMoneyListener(balance: EVMData = Constant(0),
                             balancePositive: When[Execution.Context] = Never) extends StateListener[String] {
  override def apply(state: Execution.ExecutionState) = state match {
    case FinishedState(context, true, _, contracts) =>
      val newBalance = contracts.collect{
        case (AttackerControlled(), contract) => contract.value
      }.fold(Constant(0): EVMData)(_ + _)
      SentMoneyListener(newBalance, context.implies(newBalance > 0))
    case _ => this
  }
  override def startNewTransaction(state: Execution.ExecutionState) = this
  override def interest = balancePositive match {
    case Never => NotInteresting
    case Always => Interesting(s"Balance $balance is positive")
    case Sometimes(whenYes, _) => Interesting(s"Balance $balance is positive in the following context:\n$balancePositive")
  }
}
