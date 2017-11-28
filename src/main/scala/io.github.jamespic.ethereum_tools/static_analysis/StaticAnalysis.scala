package io.github.jamespic.ethereum_tools.static_analysis

import scala.collection.mutable.{Set => MSet}
import scala.concurrent.duration._

import io.github.jamespic.ethereum_tools._


object StaticAnalysis {
  import Execution._

  sealed trait Interest[+T]
  case class Interesting[+T](result: T) extends Interest[T] with HashMemo
  case class Weird[+T](message: String) extends Interest[Nothing] with HashMemo
  case object NotInteresting extends Interest[Nothing] with HashMemo

  case class Result[T](completed: Boolean, interestingResults: Iterable[(Interest[T], FinishedState)])

  trait StateListener[+T] {
    def apply(state: ExecutionState): StateListener[T]
    def startNewTransaction(state: ExecutionState): StateListener[T]
    def interest: Interest[T]
  }

  def analyseContract[T](address: BigInt,
                         contracts: Map[EVMData, Contract],
                         listener: StateListener[T],
                         timeLimit: Option[FiniteDuration] = None
                        ): Result[T] = {
    val visitedExecutionStates = MSet.empty[ExecutionState]
    val startingState = attackState(address, contracts, 0, maxCalls = 2)
    val endTime = timeLimit map (Deadline.now + _)

    var pendingStates: Iterable[(StateListener[T], ExecutionState)] = Seq(
      (listener(startingState), startingState)
    )

    val finishedStates = MSet.empty[(Interest[T], FinishedState)]
    while (pendingStates.nonEmpty && endTime.forall(Deadline.now < _)) {
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
    Result(pendingStates.isEmpty, finishedStates)
  }

  def getContext(state: ExecutionState): Execution.Context = state match {
    case FinishedState(context, _, _, _) => context
    case AttackerContractState(calledState, _, _, _, _, _) => getContext(calledState)
    case ContractCallState(_, calledState, _, _, _) => getContext(calledState)
    case x: RunningState => x.context
  }
}