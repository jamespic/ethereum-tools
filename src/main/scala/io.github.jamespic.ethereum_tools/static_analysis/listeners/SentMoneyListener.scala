package io.github.jamespic.ethereum_tools.static_analysis.listeners

import io.github.jamespic.ethereum_tools.static_analysis._
import StaticAnalysis._
import Truthiness.truthiness
import io.github.jamespic.ethereum_tools.Bytecode._
import io.github.jamespic.ethereum_tools.static_analysis.Execution._

case class Balance(value: EVMData)
case class SentMoneyListener(balance: Balance = Balance(0)) extends StateListener[Balance] {
  override def apply(state: Execution.ExecutionState) = state match {
    case FinishedState(_, true, _, contracts) =>
      val newBalance = contracts.collect{
        case (AttackerControlled(), contract) => contract.value
      }.fold(Constant(0): EVMData)(_ + _)
      SentMoneyListener(Balance(newBalance))
    case _ => this
  }
  override def startNewTransaction(state: Execution.ExecutionState) = this
  override def interest = truthiness(balance.value > Constant(0)) match {
    case Truthy|Maybey => Interesting(balance)
    case Falsey => NotInteresting
  }
}
