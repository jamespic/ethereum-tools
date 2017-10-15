package io.github.jamespic.ethereum_tools.static_analysis

import io.github.jamespic.ethereum_tools._
import Bytecode._
import Truthiness.truthiness
import io.github.jamespic.ethereum_tools.static_analysis.listeners.SentMoneyListener

import scala.collection.SortedMap

object StaticAnalysis {
  import Execution._

  sealed trait Interest[+T]
  case class Interesting[+T](result: T) extends Interest[T] with HashMemo
  case class Weird[+T](message: String) extends Interest[Nothing] with HashMemo
  case object NotInteresting extends Interest[Nothing] with HashMemo
  trait StateListener[T] {
    def apply(state: ExecutionState): StateListener[T]
    def startNewTransaction(state: ExecutionState): StateListener[T]
    def interest: Interest[T]
  }

  def analyseContract[T](address: BigInt, contracts: Map[EVMData, Contract], listener: StateListener[T]): Iterable[(Interest[T], FinishedState)] = {
    var visitedExecutionStates = Set.empty[ExecutionState]
    var pendingStates: Iterable[(StateListener[T], ExecutionState)] =
      attackStates(address, contracts, 0, Set()) map (x => (listener(x), x))
    var finishedStates = Set.empty[(Interest[T], FinishedState)]
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
            if (lastListener.interest == NotInteresting && nextListener.interest != NotInteresting) {
              println(s"New interesting thing: ${nextListener.interest}")
              for (constraint <- nextState.constraints) println(s" - $constraint")
            }
            (nextListener, nextState)
          }
        case _ => Nil
      }
    }
    finishedStates
  }
}