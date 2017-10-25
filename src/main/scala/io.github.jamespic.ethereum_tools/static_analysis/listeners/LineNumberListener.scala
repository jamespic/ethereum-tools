package io.github.jamespic.ethereum_tools.static_analysis.listeners

import io.github.jamespic.ethereum_tools.static_analysis.Execution.{AttackerContractState, ContractCallState, ExecutionState, RunningState}
import io.github.jamespic.ethereum_tools.static_analysis.StaticAnalysis
import io.github.jamespic.ethereum_tools.static_analysis.StaticAnalysis.{Interesting, NotInteresting, StateListener}

case class LineNumberListener(lineNumber: Int, reached: Boolean = false) extends StateListener[Boolean] {
  override def apply(state: ExecutionState): StateListener[Boolean] = {
    state match {
      case x: RunningState =>
        if (x.instructionPointer == lineNumber) {
          copy(reached = true)
        } else this
      case x: ContractCallState => apply(x.calledState)
      case x: AttackerContractState => apply(x.calledState)
      case _ => this
    }
  }

  override def startNewTransaction(state: ExecutionState): StateListener[Boolean] = this

  override def interest: StaticAnalysis.Interest[Boolean] = if (reached) Interesting(true) else NotInteresting
}
