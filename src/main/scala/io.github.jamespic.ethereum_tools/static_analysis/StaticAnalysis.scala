package io.github.jamespic.ethereum_tools.static_analysis

import io.github.jamespic.ethereum_tools._

import scala.collection.mutable.{Set => MSet}

object StaticAnalysis {
  import Execution._

  sealed trait Interest[+T]
  case class Interesting[+T](result: T) extends Interest[T] with HashMemo
  case class Weird[+T](message: String) extends Interest[Nothing] with HashMemo
  case object NotInteresting extends Interest[Nothing] with HashMemo
  trait StateListener[+T] {
    def apply(state: ExecutionState): StateListener[T]
    def startNewTransaction(state: ExecutionState): StateListener[T]
    def interest: Interest[T]
  }

  def analyseContract[T](address: BigInt, contracts: Map[EVMData, Contract], listener: StateListener[T]): Iterable[(Interest[T], FinishedState)] = {
    val visitedExecutionStates = MSet.empty[ExecutionState]
    val startingState = attackState(address, contracts, 0)

    var pendingStates: Iterable[(StateListener[T], ExecutionState)] = Seq(
      (listener(startingState), startingState)
    )

    val finishedStates = MSet.empty[(Interest[T], FinishedState)]
    while (pendingStates.nonEmpty) {
      pendingStates = pendingStates flatMap {
        case (innerListener, x @ FinishedState(_, true, _, _)) =>
          val interest = innerListener.interest
          if (interest != NotInteresting) finishedStates += ((interest, x))
          Nil
        case (_, _ @ FinishedState(_, false, _, _)) => Nil
        case (lastListener, x: NonFinalExecutionState) if !(visitedExecutionStates contains x) =>
          visitedExecutionStates += x
          for (nextState <- x.nextStates) yield {
            val nextListener = lastListener(nextState)
            (nextListener, nextState)
          }
        case _ => Nil
      }
    }
    finishedStates
  }

  def getContext(state: ExecutionState): Execution.Context = state match {
    case FinishedState(context, _, _, _) => context
    case AttackerContractState(calledState, _, _, _, _) => getContext(calledState)
    case ContractCallState(_, calledState, _, _, _) => getContext(calledState)
    case x: RunningState => x.context
  }
}