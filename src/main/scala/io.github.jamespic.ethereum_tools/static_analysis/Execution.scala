package io.github.jamespic.ethereum_tools.static_analysis

import io.github.jamespic.ethereum_tools._
import Bytecode._
import Truthiness.truthiness

import scala.collection.SortedMap

object Execution {
  def reachableMapValues(key: EVMData, map: Map[EVMData, EVMData]): Seq[(EVMData, Set[Predicate])] = (for {
    (k, v) <- map
    allowedExpr = key === k
    isTruthy = truthiness(allowedExpr)
    if isTruthy != Falsey
    constraints = isTruthy match {
      case Truthy => Set.empty[Predicate]
      case Maybey => Set(allowedExpr)
      case Falsey => Set.empty[Predicate]
    }
  } yield (v, constraints)).toSeq


  case class Contract(code: Memory, storage: Map[EVMData, EVMData] = Map.empty, value: EVMData = Constant(0), nonce: Int = 0)

  val DefaultCallData = Memory(SortedMap(MemRange(0, 4096) -> CallData(0, 4096)))
  def attackerContractReturnData(returnDataSize: Int) = {
    SortedMap(MemRange(0, returnDataSize) -> AttackerReturnData(0, returnDataSize))
  }
  def attackerContractReturnData(dataLength: EVMData) = {
    val returnDataSize = dataLength match {
      case Constant(n) => n.toInt
      case _ => 0
    }
    SortedMap(MemRange(0, returnDataSize) -> AttackerReturnData(0, returnDataSize))
  }
  
  sealed trait ExecutionState {
    def recursionDepth: Int
  }
  sealed trait NonFinalExecutionState extends ExecutionState {
    def nextStates: Seq[ExecutionState]
  }
  case class FinishedState(constraints: Set[Predicate], success: Boolean,
                           result: SortedMap[MemRange, EVMData],
                           contracts: Map[EVMData, Contract]) extends ExecutionState {
    def recursionDepth = 0
  }
  case class ContractCallState(returnState: RunningState, calledState: ExecutionState,
                               returnLoc: EVMData = 0, returnSize: EVMData = 0) extends NonFinalExecutionState {
    def nextStates: Seq[ExecutionState] = {
      if (recursionDepth > 3) Nil
      else calledState match {
        case x: NonFinalExecutionState => for (nextState <- x.nextStates) yield copy(calledState = nextState)
        case FinishedState(constraints, success, result, contracts) =>
          if (success) {
            returnState.copy(
              stack = True :: returnState.stack,
              memory = returnState.memory.putRange(returnLoc, returnSize, result),
              contracts = contracts,
              constraints = returnState.constraints ++ constraints
            ) :: Nil
          } else {
            returnState.copy(
              stack = False :: returnState.stack,
              constraints = returnState.constraints ++ constraints
            ) :: Nil
          }

      }
    }
    def recursionDepth = calledState.recursionDepth + 1
  }
  def attackStates(targettedContract: EVMData, contracts: Map[EVMData, Contract],
                   returnDataSize: EVMData, constraints: Set[Predicate], attacksLeft: Int = 3) = {
    if (attacksLeft > 0) {
      val contract = contracts(targettedContract)
      Seq(
        AttackerContractState(
          targettedContract = targettedContract,
          calledState = RunningState(
            address = targettedContract,
            contracts = contracts,
            code = contract.code,
            contract = contract,
            constraints = constraints,
            callValue = Constant(0)
          ),
          returnDataSize = returnDataSize,
          attacksLeft = attacksLeft - 1
        ),
        {
          val victim = contracts(targettedContract)
          val attackerContract = contracts.getOrElse(AttackerControlledAddress, Contract(Memory()))
          val newContracts = contracts +
            (targettedContract -> victim.copy(value = victim.value + SpentMoney(1))) +
            (AttackerControlledAddress -> attackerContract.copy(value = attackerContract.value - SpentMoney(1)))
          AttackerContractState(
            targettedContract = targettedContract,
            calledState = RunningState(
              address = targettedContract,
              contracts = contracts,
              code = contract.code,
              contract = contract,
              constraints = constraints,
              callValue = Constant(0)
            ),
            returnDataSize = returnDataSize,
            attacksLeft = attacksLeft - 1
          )
        }

      )
    } else Nil
  }
  case class AttackerContractState(calledState: ExecutionState,
                                   returnDataSize: EVMData,
                                   targettedContract: EVMData,
                                   attacksLeft: Int = 3) extends NonFinalExecutionState {
    def nextStates: Seq[ExecutionState] = calledState match {
      case x: NonFinalExecutionState => for (nextState <- x.nextStates) yield copy(calledState = nextState)
      case x @ FinishedState(_, false, _, _) => Nil // If it got rolled back, we didn't do anything useful
      case x @ FinishedState(constraints, true, result, contracts) =>
        val returnValue = x.copy(result = attackerContractReturnData(returnDataSize))
        val otherActions = attackStates(targettedContract, contracts, returnDataSize, constraints, attacksLeft)
        returnValue +: otherActions
    }

    def recursionDepth = calledState.recursionDepth
  }
  case class RunningState(address: EVMData, contracts: Map[EVMData, Contract],
                          code: Memory, contract: Contract, instructionPointer: Int = 0,
                          stack: List[EVMData] = Nil, memory: Memory = Memory(),
                          sender: EVMData = AttackerControlledAddress,
                          callValue: EVMData = SpentMoney(1), constraints: Set[Predicate] = Set.empty,
                          callData: Memory = DefaultCallData, callDataLength: EVMData = CallDataLength
                     ) extends NonFinalExecutionState {

    private def simpleInstruction(modifier: PartialFunction[List[EVMData], List[EVMData]])  = {
      modifier.lift(stack) match {
        case Some(stack) => copy(stack = stack, instructionPointer = instructionPointer + 1) :: Nil
        case None => FinishedState(constraints, false, SortedMap.empty, contracts) :: Nil
      }
    }

    private def incrementIP = copy(instructionPointer = instructionPointer + 1)
    private def fail = FinishedState(constraints, false, SortedMap.empty, contracts) :: Nil
    private def withConstraint(value: EVMData) = value match {
      case x: Predicate => copy(constraints = constraints + x)
      case y => copy(constraints = constraints + Not(Equals(y, Constant(0))))
    }
    private def withAntiConstraint(value: EVMData) = value match {
      case Not(x: Predicate) => copy(constraints = constraints + x)
      case x: Predicate => copy(constraints = constraints + Not(x))
      case y => copy(constraints = constraints + Equals(y, Constant(0)))
    }
    def nextInst = decode(code.binary, instructionPointer)

    def recursionDepth = 0

    def nextStates: Seq[ExecutionState] = {
      val inst = decode(code.binary, instructionPointer)
      inst match {
        case STOP =>
          FinishedState(constraints, true, SortedMap.empty, contracts) :: Nil
        case ADD =>
          simpleInstruction {
            case a :: b :: tail => a + b :: tail
          }
        case MUL =>
          simpleInstruction {
            case a :: b :: tail => a * b :: tail
          }
        case SUB =>
          simpleInstruction {
            case a :: b :: tail => a - b :: tail
          }
        case DIV =>
          simpleInstruction {
            case a :: b :: tail => a / b :: tail
          }
        case SDIV =>
          simpleInstruction {
            case a :: b :: tail => (a sdiv b) :: tail
          }
        case MOD =>
          simpleInstruction {
            case a :: b :: tail => a % b :: tail
          }
        case SMOD =>
          simpleInstruction {
            case a :: b :: tail => (a smod b) :: tail
          }
        case ADDMOD =>
          simpleInstruction {
            case a :: b :: c :: tail => a.addmod(b, c) :: tail
          }
        case MULMOD =>
          simpleInstruction {
            case a :: b :: c :: tail => a.mulmod(b, c) :: tail
          }
        case EXP =>
          simpleInstruction {
            case a :: b :: tail => a ** b :: tail
          }
        case SIGNEXTEND =>
          simpleInstruction {
            case a :: b :: tail => b :: tail // Yes, this is obviously wrong, but let's find out if that matters
          }
        case LT =>
          simpleInstruction {
            case a :: b :: tail => (a < b) :: tail
          }
        case SLT =>
          simpleInstruction {
            case a :: b :: tail => (a slt b) :: tail
          }
        case GT =>
          simpleInstruction {
            case a :: b :: tail => (a > b) :: tail
          }
        case SGT =>
          simpleInstruction {
            case a :: b :: tail => (a sgt b) :: tail
          }
        case EQ =>
          simpleInstruction {
            case a :: b :: tail => (a === b) :: tail
          }
        case ISZERO =>
          simpleInstruction {
            case a :: tail => !a :: tail
          }
        case AND =>
          simpleInstruction {
            case a :: b :: tail => (a & b) :: tail
          }
        case OR =>
          simpleInstruction {
            case a :: b :: tail => (a | b) :: tail
          }
        case XOR =>
          simpleInstruction {
            case a :: b :: tail => (a ^ b) :: tail
          }
        case NOT =>
          simpleInstruction {
            case a :: tail => ~a :: tail
          }
        case BYTE =>
          simpleInstruction {
            case a :: Constant(b) :: tail => a.subBytes(b.toInt, b.toInt + 1) :: tail
            case a :: b :: tail => AndExpr(
              DivExpr(b,
                ExpExpr(Constant(2), MulExpr(a, Constant(8)))),
              Constant(0xff)
            ) :: tail
          }
        case SHA3 =>
          simpleInstruction {
            case Constant(a) :: Constant(b) :: tail =>
              new ConstantKeccak256(
                memory.getRange(a.toInt, b.toInt).values.toSeq,
                memory.getBinary(a.toInt, b.toInt)
              ) :: tail
            case a :: b :: tail =>
              Keccak256(
                memory.getRange(a, b).values.toSeq: _*
              ) :: tail
          }
        case ADDRESS =>
          simpleInstruction {
            case stack => address :: stack
          }
        case BALANCE =>
          simpleInstruction {
            case a :: tail if contracts contains a => contracts(a).value :: tail
            case AttackerControlledAddress :: tail => SpentMoney(1) :: tail
            case AttackerControlled() :: tail => AttackerControlled :: tail
            case _ :: tail => DefenderControlledData :: tail
          }
        case ORIGIN =>
          simpleInstruction {
            case stack => AttackerControlledAddress :: stack
          }
        case CALLER =>
          simpleInstruction {
            case stack => sender :: stack
          }
        case CALLVALUE =>
          simpleInstruction {
            case stack => callValue :: stack
          }
        case CALLDATALOAD =>
          simpleInstruction {
            case a :: tail => callData.get(a) :: tail
          }
        case CALLDATASIZE =>
          simpleInstruction {
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
          simpleInstruction {
            case stack => Constant(code.binary.length) :: stack
          }
        case CODECOPY =>
          stack match {
            case memOffset :: codeOffset :: length :: tail =>
              val newMemory = memory.putRange(memOffset, length, code.getRange(codeOffset, length))
              copy(memory = newMemory, stack = tail).incrementIP :: Nil
            case _ => fail
          }
        case GASPRICE =>
          simpleInstruction {
            case stack => AttackerControlled :: stack
          }
        case EXTCODESIZE =>
          simpleInstruction {
            case addr :: stack if contracts contains addr => contracts(addr).code.binary.length :: stack
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
          simpleInstruction {
            case _ :: tail => DefenderControlledData :: tail
          }
        case COINBASE =>
          simpleInstruction {
            case stack => DefenderControlledAddress :: stack
          }
        case TIMESTAMP =>
          simpleInstruction {
            case stack => Timestamp :: stack
          }
        case NUMBER =>
          simpleInstruction {
            case stack => Blocknumber :: stack
          }
        case DIFFICULTY =>
          simpleInstruction {
            case stack => DefenderControlledData :: stack
          }
        case GASLIMIT =>
          simpleInstruction {
            case stack => DefenderControlledData :: stack
          }
        case POP =>
          simpleInstruction {
            case _ :: tail => tail
          }
        case MLOAD =>
          simpleInstruction {
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
            case Constant(a) :: tail if decode(code.binary, a.toInt) == JUMPDEST =>
              copy(instructionPointer = a.toInt, stack = tail) :: Nil
            case _ => fail
          }
        case JUMPI =>
          stack match {
            case Constant(a) :: v :: tail if decode(code.binary, a.toInt) == JUMPDEST =>
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
          simpleInstruction {
            case stack => DefenderControlledData :: stack
          }
        case GAS =>
          simpleInstruction {
            case stack => AttackerControlled :: stack
          }
        case JUMPDEST => incrementIP :: Nil
        case PUSH(l, _) =>
          // Distrust value decoded from binary, because binary is incomplete. Get from code directly
          val decodedValue = code.getSingleValueFromRange(instructionPointer + 1, l)
          copy(instructionPointer = instructionPointer + 1 + l, stack = decodedValue :: stack) :: Nil
        case DUP(n) =>
          simpleInstruction {
            case stack if stack.length >= n => stack(n - 1) :: stack
          }
        case SWAP(n) =>
          simpleInstruction {
            case head :: tail if tail.length >= n => tail(n - 1) :: tail.take(n - 1) ::: head  :: tail.drop(n)
          }
        case LOG(n) =>
          simpleInstruction {
            case stack if stack.length >= n + 2 => stack.drop(n + 2)
          }
        case CREATE =>
          stack match {
            case value :: codeStart :: codeLength :: tail =>
              def newContract = {
                val newContractAddress = NewContractAddress(address, contract.nonce)
                val code = Memory(memory.getRange(codeStart, codeLength))
                val newContract = Contract(code, value = value)
                copy(
                  contracts = contracts + (address -> contract.copy(
                    value = contract.value - value,
                    nonce = contract.nonce + 1
                  )) + (newContractAddress -> newContract)
                ).incrementIP
              }
              truthiness(value > contract.value) match {
                case Truthy =>
                  newContract :: Nil
                case Falsey =>
                  fail
                case Maybey =>
                  newContract.withConstraint(value > contract.value) ::
                  FinishedState(constraints + (value < contract.value), false, SortedMap.empty, contracts) :: Nil
              }
            case _ => fail
          }
        case CALL =>
          stack match {
            case gas :: to :: value :: dataOffset :: dataLength :: returnOffset :: returnLength :: tail =>
              val toContract = contracts.getOrElse(to, Contract(Memory()))
              val newContracts = contracts +
                (address -> contract.copy(value = contract.value - value)) +
                (to -> toContract.copy(value = toContract.value + value))
              val enoughMoney = truthiness(value < contract.value)
              val extraConstraints = enoughMoney match {
                case Truthy|Falsey => Set()
                case Maybey => Set(value < contract.value)
              }
              val callData = Memory(memory.getRange(dataOffset, dataLength))
              val contractCalls: Seq[ExecutionState] = to match {
                case AttackerControlled() =>
                  FinishedState(
                    constraints = constraints ++ extraConstraints,
                    success = true,
                    result = attackerContractReturnData(returnLength),
                    contracts = newContracts) +:
                  attackStates(targettedContract = address,
                    contracts = newContracts,
                    returnDataSize = returnLength,
                    constraints = constraints ++ extraConstraints)
                case to if contracts contains to =>
                  // Forward to other contract
                  Seq(RunningState(
                    address = to,
                    contracts = newContracts,
                    contract = newContracts(address),
                    code = newContracts(address).code,
                    sender = address,
                    callValue = value,
                    callData = callData,
                    constraints = constraints ++ extraConstraints
                  ))
                case Constant(n) if PrecompiledContracts.Contracts contains n.toInt =>
                  PrecompiledContracts.Contracts(n.toInt)(callData, callDataLength) map {result =>
                    result.copy(
                      constraints = result.constraints ++ constraints ++ extraConstraints,
                      contracts = newContracts
                    )
                  }
                case _ =>
                  // Not a contract, just send money
                  Seq(FinishedState(constraints ++ extraConstraints, true, SortedMap.empty, newContracts))
              }
              (for (calledState <- contractCalls if enoughMoney != Falsey) yield {
                ContractCallState(
                  returnState = this.copy(
                    stack = tail
                  ).incrementIP,
                  calledState = calledState,
                  returnLoc = returnOffset,
                  returnSize = returnLength
                )
              }) :+ this.copy(stack = False :: stack).incrementIP
            case _ => fail
          }
        case CALLCODE =>
          stack match {
            case gas :: to :: value :: dataOffset :: dataLength :: returnOffset :: returnLength :: tail =>
              val toContract = contracts.getOrElse(to, Contract(Memory()))
              val callData = Memory(memory.getRange(dataOffset, dataLength))
              val contractCalls: Seq[ExecutionState] = to match {
                case AttackerControlled() =>
                  // The defender is boned at this point
                  val newContracts = contracts +
                    (address -> contract.copy(value = Constant(0))) +
                    (AttackerControlledAddress -> {
                      val attackerAccount = contracts.getOrElse(AttackerControlledAddress, Contract(Memory()))
                      attackerAccount.copy(value = attackerAccount.value + contract.value)
                    })
                  Seq(FinishedState(Set.empty, true, attackerContractReturnData(returnLength), newContracts))
                case to if contracts contains to =>
                  // Forward to other contract
                  Seq(RunningState(
                    address = address,
                    contracts = contracts,
                    contract = contract,
                    code = toContract.code,
                    sender = address,
                    callValue = value,
                    callData = callData,
                    constraints = constraints
                  ))
                case Constant(n) if PrecompiledContracts.Contracts contains n.toInt =>
                  PrecompiledContracts.Contracts(n.toInt)(callData, callDataLength) map {result =>
                    result.copy(
                      constraints = result.constraints ++ constraints,
                      contracts = contracts
                    )
                  }
                case _ =>
                  // A weird thing to do, but strictly valid
                  Seq(FinishedState(constraints, true, SortedMap.empty, contracts))
              }
              (for (calledState <- contractCalls) yield {
                ContractCallState(
                  returnState = this.copy(
                    stack = tail
                  ).incrementIP,
                  calledState = calledState,
                  returnLoc = returnOffset,
                  returnSize = returnLength
                )
              }) :+ this.copy(stack = False :: stack).incrementIP
            case _ => fail
          }
        case DELEGATECALL =>
          stack match {
            case gas :: to :: dataOffset :: dataLength :: returnOffset :: returnLength :: tail =>
              val toContract = contracts.getOrElse(to, Contract(Memory()))
              val callData = Memory(memory.getRange(dataOffset, dataLength))
              val contractCalls: Seq[ExecutionState] = to match {
                case AttackerControlled() =>
                  // The defender is boned at this point
                  val newContracts = contracts +
                    (address -> contract.copy(value = Constant(0))) +
                    (AttackerControlledAddress -> {
                      val attackerAccount = contracts.getOrElse(AttackerControlledAddress, Contract(Memory()))
                      attackerAccount.copy(value = attackerAccount.value + contract.value)
                    })
                  Seq(FinishedState(Set.empty, true, attackerContractReturnData(returnLength), newContracts))
                case to if contracts contains to =>
                  // Forward to other contract
                  Seq(RunningState(
                    address = address,
                    contracts = contracts,
                    contract = contract,
                    code = toContract.code,
                    sender = sender,
                    callValue = callValue,
                    callData = callData,
                    constraints = constraints
                  ))
                case Constant(n) if PrecompiledContracts.Contracts contains n.toInt =>
                  PrecompiledContracts.Contracts(n.toInt)(callData, callDataLength) map {result =>
                    result.copy(
                      constraints = result.constraints ++ constraints,
                      contracts = contracts
                    )
                  }
                case _ =>
                  // A weird thing to do, but strictly valid
                  Seq(FinishedState(constraints, true, SortedMap.empty, contracts))
              }
              (for (calledState <- contractCalls) yield {
                ContractCallState(
                  returnState = this.copy(
                    stack = tail
                  ).incrementIP,
                  calledState = calledState,
                  returnLoc = returnOffset,
                  returnSize = returnLength
                )
              }) :+ this.copy(stack = False :: stack).incrementIP
            case _ => fail
          }
        case RETURN =>
          stack match {
            case a :: b :: tail =>
              Seq(FinishedState(constraints, true, memory.getRange(a, b), contracts))
          }
        case UNKNOWN|INVALID|REVERT =>
          Seq(FinishedState(constraints, false, SortedMap.empty, contracts))
        case SELFDESTRUCT =>
          stack match {
            case a :: tail =>
              val recipient = contracts.getOrElse(a, Contract(Memory()))
              Seq(
                FinishedState(constraints, true, SortedMap.empty,
                  contracts - address + (a -> recipient.copy(value = contract.value))),
                FinishedState(constraints, false, SortedMap.empty, contracts) // Yes, SELFDESTRUCT can fail
              )
          }
      }
    }
  }
}
