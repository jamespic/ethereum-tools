package io.github.jamespic.ethereum_tools.static_analysis.listeners

import io.github.jamespic.ethereum_tools.static_analysis.Execution.ExecutionState
import io.github.jamespic.ethereum_tools.static_analysis.StaticAnalysis
import io.github.jamespic.ethereum_tools.static_analysis.StaticAnalysis.{NotInteresting, StateListener}

case class HistoryListener(history: List[ExecutionState] = Nil) extends StateListener[Nothing] {
  override def apply(state: ExecutionState): StateListener[Nothing] = copy(history = state :: history)

  override def startNewTransaction(state: ExecutionState): StateListener[Nothing] = this

  override def interest: StaticAnalysis.Interest[Nothing] = NotInteresting
}
