package io.github.jamespic.ethereum_tools.static_analysis

import io.github.jamespic.ethereum_tools._
import Bytecode._

import scala.collection.SortedMap

object StaticAnalysis {


  def reachableMapValues(key: EVMData, map: Map[EVMData, EVMData]): Seq[(EVMData, Set[Predicate])] = key match {
    case AttackerControlled() =>
      for ((k, v) <- map.toSeq) yield (v, Set(Equals(key, k): Predicate))
    case _ =>
      (
        for ((k @ AttackerControlled(), v) <- map.toSeq)
          yield (v, Set(Equals(key, k): Predicate))
      ) :+ (map.getOrElse(key, DefenderControlledData) -> Set.empty[Predicate])
  }


  sealed trait Truthiness
  case object Truthy extends Truthiness
  case object Falsey extends Truthiness
  case object Maybey extends Truthiness

  def truthiness(predicate: EVMData): Truthiness = predicate match {
    case Constant(x) => if (x == 0) Falsey else Truthy
    case Equals(AttackerControlledAddress, DefenderControlled()|Constant(_)) => Falsey
    case Equals(DefenderControlled(), Constant(_)) => Falsey
    case Not(x) => truthiness(x) match {
      case Truthy => Falsey
      case Falsey => Truthy
      case Maybey => Maybey
    }
    case AndExpr(a: Predicate, b: Predicate) => (truthiness(a), truthiness(b)) match {
      case (Falsey, _)|(_, Falsey) => Falsey
      case (Maybey, Truthy|Maybey)|(Truthy|Maybey, Maybey) => Maybey
      case (Truthy, Truthy) => Truthy
    }
    case OrExpr(a: Predicate, b: Predicate) => (truthiness(a), truthiness(b)) match {
      case (Truthy, _)|(_, Truthy) => Truthy
      case (Maybey, Falsey|Maybey)|(Falsey|Maybey, Maybey) => Maybey
      case (Falsey, Falsey) => Falsey
    }
    case _ => Maybey
  }

  case class Contract(address: BigInt, code: Memory, storage: Map[EVMData, EVMData], value: EVMData)

  type MemoryExpr = Map[Int, EVMData]
  val DefaultCallData = Memory(SortedMap(MemRange(0, 4096) -> CallData(0, 4096)))
  sealed trait ExecutionState
  case class FinishedState(constraints: Set[Predicate], success: Boolean,
                           result: MemoryExpr, contracts: Map[EVMData, Contract]) extends ExecutionState
  case class ContractCallState(returnState: RunningState, calledState: ExecutionState,
                               returnMemoryLoc: Option[(EVMData, EVMData)] = None) extends ExecutionState {
    def nextStates: Seq[ExecutionState] = calledState match {
      case x: RunningState => for (nextState <- x.nextStates) yield ContractCallState(returnState, nextState)
      case x: ContractCallState => for (nextState <- x.nextStates) yield ContractCallState(returnState, nextState)
      case x: FinishedState => ??? // FIXME: Copy result into returnState
    }
  }
  case class RunningState(address: EVMData, contracts: Map[EVMData, Contract], instructionPointer: Int = 0,
                          stack: List[EVMData] = Nil, memory: Memory = Memory(),
                          sender: EVMData = AttackerControlledAddress,
                          callValue: EVMData = SpentMoney(1), constraints: Set[Predicate] = Set.empty,
                          callData: Memory = DefaultCallData, callDataLength: EVMData = CallDataLength
                     ) extends ExecutionState {
    def contract = contracts(address)

    private def modifyStack(modifier: PartialFunction[List[EVMData], List[EVMData]])  = {
      modifier.lift(stack) match {
        case Some(stack) => copy(stack = stack) :: Nil
        case None => FinishedState(constraints, false, Map.empty, contracts) :: Nil
      }
    }
    
    private def incrementIP = copy(instructionPointer = instructionPointer + 1)
    private def fail = FinishedState(constraints, false, Map.empty, contracts) :: Nil
    private def withConstraint(value: EVMData) = value match {
      case x: Predicate => copy(constraints = constraints + x)
      case y => copy(constraints = constraints + Not(Equals(y, Constant(0))))
    }
    private def withAntiConstraint(value: EVMData) = value match {
      case Not(x: Predicate) => copy(constraints = constraints + x)
      case x: Predicate => copy(constraints = constraints + Not(x))
      case y => copy(constraints = constraints + Equals(y, Constant(0)))
    }

    def nextStates: Seq[ExecutionState] = {
      val inst = decode(contract.code.binary, instructionPointer)
      inst match {
        case STOP =>
          FinishedState(constraints, true, Map.empty, contracts) :: Nil
        case ADD =>
          incrementIP.modifyStack {
            case a :: b :: tail => a + b :: tail
          }
        case MUL =>
          incrementIP.modifyStack {
            case a :: b :: tail => a * b :: tail
          }
        case SUB =>
          incrementIP.modifyStack {
            case a :: b :: tail => a - b :: tail
          }
        case DIV =>
          incrementIP.modifyStack {
            case a :: b :: tail => a / b :: tail
          }
        case SDIV =>
          incrementIP.modifyStack {
            case a :: b :: tail => (a sdiv b) :: tail
          }
        case MOD =>
          incrementIP.modifyStack {
            case a :: b :: tail => a % b :: tail
          }
        case SMOD =>
          incrementIP.modifyStack {
            case a :: b :: tail => (a smod b) :: tail
          }
        case ADDMOD =>
          incrementIP.modifyStack {
            case a :: b :: c :: tail => a.addmod(b, c) :: tail
          }
        case MULMOD =>
          incrementIP.modifyStack {
            case a :: b :: c :: tail => a.mulmod(b, c) :: tail
          }
        case EXP =>
          incrementIP.modifyStack {
            case a :: b :: tail => a ** b :: tail
          }
        case SIGNEXTEND =>
          incrementIP.modifyStack {
            case a :: b :: tail => b :: tail // Yes, this is obviously wrong, but let's find out if that matters
          }
        case LT =>
          incrementIP.modifyStack {
            case a :: b :: tail => (a < b) :: tail
          }
        case SLT =>
          incrementIP.modifyStack {
            case a :: b :: tail => (a slt b) :: tail
          }
        case GT =>
          incrementIP.modifyStack {
            case a :: b :: tail => (a > b) :: tail
          }
        case SGT =>
          incrementIP.modifyStack {
            case a :: b :: tail => (a sgt b) :: tail
          }
        case EQ =>
          incrementIP.modifyStack {
            case a :: b :: tail => (a === b) :: tail
          }
        case ISZERO =>
          incrementIP.modifyStack {
            case a :: tail => !a :: tail
          }
        case AND =>
          incrementIP.modifyStack {
            case a :: b :: tail => (a & b) :: tail
          }
        case OR =>
          incrementIP.modifyStack {
            case a :: b :: tail => (a | b) :: tail
          }
        case XOR =>
          incrementIP.modifyStack {
            case a :: b :: tail => (a ^ b) :: tail
          }
        case NOT =>
          incrementIP.modifyStack {
            case a :: tail => ~a :: tail
          }
        case BYTE =>
          incrementIP.modifyStack {
            case a :: Constant(b) :: tail => a.subBytes(b.toInt, b.toInt + 1) :: tail
            case a :: b :: tail => AndExpr(
              DivExpr(b,
                ExpExpr(Constant(2), MulExpr(a, Constant(8)))),
              Constant(0xff)
            ) :: tail
          }
        case SHA3 =>
          incrementIP.modifyStack {
            // FIXME: Fix this once you've got a good model for memory
            case Constant(a) :: Constant(b) :: tail =>
              new ConstantKeccak256(
                memory.getRange(a.toInt, b.toInt).values.toSeq,
                memory.getBinary(a.toInt, b.toInt)
              ) :: Nil
            case a :: b :: tail =>
              Keccak256(
                memory.getRange(a, b).values.toSeq: _*
              ) :: Nil
          }
        case ADDRESS =>
          incrementIP.modifyStack {
            case stack => address :: stack
          }
        case BALANCE =>
          incrementIP.modifyStack {
            case a :: tail if contracts contains a => contracts(a).value :: tail
            case AttackerControlledAddress :: tail => SpentMoney(1) :: tail
            case AttackerControlled() :: tail => AttackerControlled :: tail
            case _ :: tail => DefenderControlledData :: tail
          }
        case ORIGIN =>
          incrementIP.modifyStack {
            case stack => AttackerControlledAddress :: stack
          }
        case CALLER =>
          incrementIP.modifyStack {
            case stack => sender :: stack
          }
        case CALLVALUE =>
          incrementIP.modifyStack {
            case stack => callValue :: stack
          }
        case CALLDATALOAD =>
          incrementIP.modifyStack {
            case a :: tail => callData.get(a) :: tail
          }
        case CALLDATASIZE =>
          incrementIP.modifyStack {
            case tail => callDataLength :: tail
          }
        case CALLDATACOPY =>
          stack match {
            case memOffset :: callDataOffset :: length :: tail =>
              val newMemory = memory.putRange(memOffset, length, callData.getRange(callDataOffset, length))
              copy(memory = newMemory, stack = tail).incrementIP :: Nil
            case _ => fail
          }
        case CODESIZE =>
          incrementIP.modifyStack {
            case stack => Constant(contract.code.binary.length) :: stack
          }
        case CODECOPY =>
          stack match {
            case memOffset :: codeOffset :: length :: tail =>
              val newMemory = memory.putRange(memOffset, length, contract.code.getRange(codeOffset, length))
              copy(memory = newMemory, stack = tail).incrementIP :: Nil
            case _ => fail
          }
        case GASPRICE =>
          incrementIP.modifyStack {
            case stack => AttackerControlled :: stack
          }
        case EXTCODESIZE =>
          incrementIP.modifyStack {
            case addr :: stack if contracts contains addr => Constant(contracts(addr).code.binary.length) :: stack
            case AttackerControlled() :: stack => AttackerControlled :: stack
            case _ :: stack => DefenderControlledData :: stack
          }
        case EXTCODECOPY =>
          // FIXME: Fix this once there's a good model for memory
          stack match {
            case addr :: Constant(memOffset) :: Constant(codeOffset) :: Constant(length) :: tail if contracts contains addr =>
              val newMemory = memory.putRange(memOffset, length, contracts(addr).code.getRange(codeOffset, length))
              copy(memory = newMemory, stack=tail).incrementIP :: Nil
            case _ => fail
          }
        case BLOCKHASH =>
          incrementIP.modifyStack {
            case _ :: tail => DefenderControlledData :: tail
          }
        case COINBASE =>
          incrementIP.modifyStack {
            case stack => DefenderControlledAddress :: stack
          }
        case TIMESTAMP =>
          incrementIP.modifyStack {
            case stack => Timestamp :: stack
          }
        case NUMBER =>
          incrementIP.modifyStack {
            case stack => Blocknumber :: stack
          }
        case DIFFICULTY =>
          incrementIP.modifyStack {
            case stack => DefenderControlledData :: stack
          }
        case GASLIMIT =>
          incrementIP.modifyStack {
            case stack => DefenderControlledData :: stack
          }
        case POP =>
          incrementIP.modifyStack {
            case _ :: tail => tail
          }
        case MLOAD =>
          incrementIP.modifyStack {
            // Consider handling possibility of attacker controlled a
            case a :: tail => memory.get(a) :: tail
          }
        case MSTORE =>
          // FIXME: Fix this once there's a good model for memory
          stack match {
            case a :: b :: tail => copy(memory = memory.put(a, b), stack = tail).incrementIP :: Nil
            case _ => fail
          }
        case MSTORE8 =>
          // FIXME: Fix this once there's a good model for memory
          stack match {
            case a :: b :: tail => copy(
              memory = memory.putRange(a, 1, Seq(MemRange(0, 1) -> b.clipHighBytes(1))),
              stack = tail
            ).incrementIP :: Nil
            case _ => fail
          }
        case SLOAD =>
          stack match {
            case a :: tail =>
              for ((v, newConstraints) <- reachableMapValues(a, contract.storage)) yield {
                copy(stack = v :: tail, constraints = constraints ++ newConstraints).incrementIP
              }
            case _ => fail
          }
        case SSTORE =>
          stack match {
            case a :: b :: tail =>
              val newContract = contract.copy(storage = contract.storage + (a -> b))
              copy(contracts = contracts + (address -> newContract), stack = tail).incrementIP :: Nil
            case _ => fail
          }
        case JUMP =>
          stack match {
            case Constant(a) :: tail if decode(contract.code.binary, a.toInt) == JUMPDEST =>
              copy(instructionPointer = a.toInt, stack = tail) :: Nil
            case _ => fail
          }
        case JUMPI =>
          stack match {
            case v :: Constant(a) :: tail if decode(contract.code.binary, a.toInt) == JUMPDEST =>
              truthiness(v) match {
                case Truthy => copy(instructionPointer = a.toInt, stack = tail) :: Nil
                case Falsey => copy(stack = tail).incrementIP :: Nil
                case Maybey =>
                  copy(instructionPointer = a.toInt, stack = tail).withConstraint(v) ::
                    copy(stack = tail).incrementIP.withAntiConstraint(v) :: Nil
              }
            case _ => fail
          }
        case PC =>
          copy(stack = Constant(instructionPointer) :: stack).incrementIP :: Nil
        case MSIZE =>
          incrementIP.modifyStack {
            case stack => DefenderControlledData :: stack
          }
        case GAS =>
          incrementIP.modifyStack {
            case stack => AttackerControlled :: stack
          }
        case JUMPDEST => incrementIP :: Nil
        case PUSH(l, data) =>
          // Distrust value decoded from binary, because binary is incomplete. Get from code directly
          val decodedValue = contract.code.getSingleValueFromRange(instructionPointer + 1, instructionPointer + 1 + l)
          copy(instructionPointer = instructionPointer + 1 + l, stack = decodedValue :: stack) :: Nil
        case DUP(n) =>
          incrementIP.modifyStack {
            case stack if stack.length >= n => stack(n - 1) :: stack
          }
        case SWAP(n) =>
          incrementIP.modifyStack {
            case stack if stack.length >= n + 1 => stack(n) :: stack.take(n) ::: stack.drop(n + 1)
          }
        case LOG(n) =>
          incrementIP.modifyStack {
            case stack if stack.length >= n + 2 => stack.drop(n + 2)
          }
        case CREATE => ???
          // FIXME: Implement me
      }
    }

  }

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
    walkExecutionState(RunningState(Constant(contract), contracts))(listener)
  }
}