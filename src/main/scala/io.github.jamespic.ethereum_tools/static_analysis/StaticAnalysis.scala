package io.github.jamespic.ethereum_tools.static_analysis

import io.github.jamespic.ethereum_tools._
import Bytecode._
import Truthiness.truthiness

import scala.collection.SortedMap

object StaticAnalysis {
  import Execution._

  sealed trait Interest
  case class Interesting(message: String) extends Interest
  case class Weird(message: String) extends Interest
  case object NotInteresting extends Interest
  trait StateListener {
    def apply(state: ExecutionState): StateListener
    def interest: Interest
  }

  def analyseContract(contract: BigInt, contracts: Map[EVMData, Contract])(listener: StateListener): Stream[(Interest, FinishedState)] = {
    var visitedExecutionStates = Set.empty[ExecutionState]
    def walkExecutionState(executionState: ExecutionState)(listener: StateListener): Stream[(Interest, FinishedState)] = {
      if (visitedExecutionStates contains executionState) Stream.empty
      else {
        visitedExecutionStates += executionState
        executionState match {
          case x @ FinishedState(_, true, _, _) => Stream((listener.interest, x))
          case x: RunningState =>
            x.nextStates.toStream.flatMap {state =>
              walkExecutionState(state)(listener(state))
            }
          case _ => Stream.empty
        }
      }
    }
    walkExecutionState(RunningState(
      address = contract,
      code = contracts(contract).code,
      contract = contracts(contract),
      contracts = contracts
    ))(listener)
  }
}