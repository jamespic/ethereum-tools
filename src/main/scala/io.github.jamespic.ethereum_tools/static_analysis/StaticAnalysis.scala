package io.github.jamespic.ethereum_tools.static_analysis

import io.github.jamespic.ethereum_tools._
import Bytecode._
import Truthiness.truthiness
import io.github.jamespic.ethereum_tools.static_analysis.listeners.SentMoneyListener

import scala.collection.SortedMap

object StaticAnalysis {
  import Execution._

  sealed trait Interest[+T]
  case class Interesting[+T](result: T) extends Interest[T]
  case class Weird[+T](message: String) extends Interest[Nothing]
  case object NotInteresting extends Interest[Nothing]
  trait StateListener[T] {
    def apply(state: ExecutionState): StateListener[T]
    def startNewTransaction(state: ExecutionState): StateListener[T]
    def interest: Interest[T]
  }

//  def analyseContract[T](address: BigInt, contracts: Map[EVMData, Contract], listener: StateListener[T]): Stream[(Interest[T], FinishedState)] = {
//    var visitedExecutionStates = Set.empty[ExecutionState]
//    def walkExecutionState(executionState: ExecutionState, listener: StateListener[T]): Stream[(Interest[T], FinishedState)] = {
//      if (visitedExecutionStates contains executionState) Stream.empty
//      else {
//        visitedExecutionStates += executionState
//        executionState match {
//          case x: NonFinalExecutionState =>
//            x.nextStates.toStream.flatMap {state =>
//              walkExecutionState(state, listener(state))
//            }
//          case x @ FinishedState(_, true, _, _) => Stream((listener(x).interest, x))
//          case x @ FinishedState(_, false, _, _) => Stream.empty
//        }
//      }
//    }
//    val startState = RunningState(
//      address = address,
//      code = contracts(address).code,
//      contract = contracts(address),
//      contracts = contracts
//    )
//    walkExecutionState(startState, listener.startNewTransaction(startState))
//  }

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
              println(s"New interesting thing: ${nextListener.interest} at ${nextState}")
            }
            (nextListener, nextState)
          }
        case _ => Nil
      }
    }
    finishedStates
  }
}