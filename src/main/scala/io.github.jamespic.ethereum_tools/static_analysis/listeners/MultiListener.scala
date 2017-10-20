package io.github.jamespic.ethereum_tools.static_analysis.listeners

import io.github.jamespic.ethereum_tools.static_analysis.Execution.ExecutionState
import io.github.jamespic.ethereum_tools.static_analysis.{Execution, StaticAnalysis}
import io.github.jamespic.ethereum_tools.static_analysis.StaticAnalysis.{Interesting, NotInteresting, StateListener}

case object MultiListener {
  def apply(things: (String, StateListener[Any])*): MultiListener = MultiListener(things.toMap)
}
case class MultiListener(listeners: Map[String, StateListener[Any]]) extends StateListener[Map[String, Any]] {
  override def apply(state: ExecutionState) = MultiListener(
    for ((k, v) <- listeners) yield k -> v(state)
  )
  override def startNewTransaction(state: Execution.ExecutionState) = MultiListener(
    for ((k, v) <- listeners) yield k -> v.startNewTransaction(state)
  )

  override def interest: StaticAnalysis.Interest[Map[String, Any]] = {
    if (listeners.exists(_._2.interest != NotInteresting)) {
      Interesting(
        for {
          (k, v) <- listeners
          if v.interest.isInstanceOf[Interesting[_]]
          Interesting(x) = v.interest
        } yield k -> x
      )
    } else NotInteresting
  }
}
