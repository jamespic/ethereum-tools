package io.github.jamespic.ethereum_tools.static_analysis

import io.github.jamespic.ethereum_tools._
import Bytecode._
import io.github.jamespic.ethereum_tools.static_analysis.constraints._

import scala.collection.SortedMap

object Execution {
  def reachableMapValues(key: EVMData, map: Map[EVMData, EVMData], context: Context): Seq[(EVMData, Context)] = {
    val results = map flatMap {
      case (k, v) =>
        context.implies(key === k) match {
          case Always => Seq((v, context))
          case Sometimes(yesWhen, _) => yesWhen.toSeq.map(ctx => (v, ctx))
          case Never => Nil
        }
    }
    if (results.nonEmpty) results.toSeq else Seq((Constant(0), context))
  }


  case class Contract(code: MemoryLike, storage: Map[EVMData, EVMData] = Map.empty,
                      value: EVMData = Constant(0), nonce: Int = 0) extends HashMemo

  def defaultCallData(callId: Int = 0) = Memory(SortedMap(MemRange(0, 4096) -> CallData(0, 4096, callId)))
  def attackerContractReturnData(returnDataSize: Int, callId: Int) = {
    SortedMap(MemRange(0, returnDataSize) -> AttackerReturnData(0, returnDataSize, callId))
  }
  def attackerContractReturnData(dataLength: EVMData, callId: Int) = AttackerReturnData(0, dataLength, callId)

  val maxCalls = 2

  def estimateBlockNumber(timestamp: Long) = {
    (timestamp - 1509310839249L) / 15000L + 4453849
  }

  case class Context(constraints: EVMConstraints = EVMConstraints(),
                     callCount: Int = 0,
                     timestamp: Long = System.currentTimeMillis() / 1000,
                     blockNumber: Long = estimateBlockNumber(System.currentTimeMillis)
                    ) {
    def incrementCalls = copy(callCount = callCount + 1)
    def addNonNegativityConstraint(m: AttackerControlled) = {
      copy(constraints =
        constraints.copy(linearConstraints = constraints.linearConstraints.addNonNegativityConstraint(m))
      )
    }
    def implies(predicate: EVMData) = constraints.implies(predicate).map(x => copy(constraints = x))
    override def toString =
      s"""
         |Linear Constraints
         |------------------
         |${constraints.linearConstraints}
         |
         |Other Constraints
         |-----------------
         |${constraints.otherConstraints.mkString("\n")}
         |
         |Call Count: $callCount
      |""".stripMargin
    def mkTimestamp = new Timestamp(timestamp)
    def mkBlockNumber = new Blocknumber(blockNumber)

  }
  sealed trait ExecutionState
  sealed trait NonFinalExecutionState extends ExecutionState {
    def nextStates: Seq[ExecutionState]
  }
  case class FinishedState(context: Context, success: Boolean,
                           result: MemoryLike,
                           contracts: Map[EVMData, Contract]) extends ExecutionState with HashMemo {
  }
  case class ContractCallState(returnState: RunningState, calledState: ExecutionState,
                               returnLoc: EVMData = 0, returnSize: EVMData = 0,
                               context: Context = Context()) extends NonFinalExecutionState with HashMemo {
    def nextStates: Seq[ExecutionState] = {
      calledState match {
        case x: NonFinalExecutionState if context.callCount < maxCalls =>
          for (nextState <- x.nextStates) yield copy(calledState = nextState)
        case FinishedState(context, true, result, contracts) =>
            returnState.copy(
              stack = True :: returnState.stack,
              memory = returnState.memory.putRange(returnLoc, returnSize, result.getRange(0, returnSize)),
              context = context,
              contracts = contracts
            ) :: Nil
        case _ =>
          returnState.copy(
            stack = False :: returnState.stack,
            context = context
          ) :: Nil

      }
    }
  }

  def attackState(targettedContract: EVMData, contracts: Map[EVMData, Contract], returnDataSize: EVMData,
                  context: Context = Context(), maxCalls: Int = maxCalls): ExecutionState = {
    contracts.get(targettedContract) match {
      case Some(victim) =>
        val attackerContract = contracts.getOrElse(AttackerControlledAddress, Contract(Memory()))
        val callValue = SpentMoney(context.callCount)
        val newContracts = contracts +
          (targettedContract -> victim.copy(value = victim.value + callValue)) +
          (AttackerControlledAddress -> attackerContract.copy(value = attackerContract.value - callValue))
        val callDataLength = CallDataLength(context.callCount)
        val newContext = context.addNonNegativityConstraint(callValue).addNonNegativityConstraint(callDataLength)
        AttackerContractState(
          targettedContract = targettedContract,
          calledState = RunningState(
            address = targettedContract,
            contracts = newContracts,
            code = victim.code,
            context = newContext,
            callData = defaultCallData(context.callCount),
            callDataLength = callDataLength,
            callValue = callValue
          ),
          returnDataSize = returnDataSize,
          context = newContext,
          startContracts = contracts,
          maxCalls = maxCalls
        )
      case None =>
        FinishedState(context, true, Memory(), contracts) // Can happen if contract self-destructs
    }
  }
  case class AttackerContractState(calledState: ExecutionState,
                                   returnDataSize: EVMData,
                                   targettedContract: EVMData,
                                   context: Context,
                                   startContracts: Map[EVMData, Contract],
                                   maxCalls: Int = maxCalls) extends NonFinalExecutionState with HashMemo {
    def nextStates: Seq[ExecutionState] = calledState match {
      case x: NonFinalExecutionState =>
        for (nextState <- x.nextStates) yield copy(calledState = nextState)
      case x @ FinishedState(_, false, _, _) => Nil // If it got rolled back, we didn't do anything useful
      case x @ FinishedState(context, true, _, contracts) =>
        if (contracts == startContracts) Nil // This did nothing interesting
        else {
          val returnValue = x.copy(result = attackerContractReturnData(returnDataSize, context.callCount))
          val otherActions = if (context.callCount < maxCalls) {
            Seq(
              attackState(
                targettedContract = targettedContract,
                contracts = contracts,
                returnDataSize = returnDataSize,
                context = context.incrementCalls
              )
            )
          } else Nil
          returnValue +: otherActions
        }
    }
  }
  case class RunningState(address: EVMData, contracts: Map[EVMData, Contract],
                          code: MemoryLike, instructionPointer: Int = 0,
                          stack: List[EVMData] = Nil, memory: MemoryLike = Memory(),
                          sender: EVMData = AttackerControlledAddress,
                          callValue: EVMData = Constant(0), context: Context = Context(),
                          callData: MemoryLike = defaultCallData(), callDataLength: EVMData = CallDataLength(0)
                     ) extends NonFinalExecutionState with HashMemo {
    lazy val contract = contracts(address)
    private def simpleInstruction(modifier: PartialFunction[List[EVMData], List[EVMData]])  = {
      modifier.lift(stack) match {
        case Some(stack) => copy(stack = stack, instructionPointer = instructionPointer + 1) :: Nil
        case None => fail
      }
    }
    def balance = contracts.get(address).map(_.value).getOrElse(Constant(0))

    private def incrementIP = copy(instructionPointer = instructionPointer + 1)
    private def fail = FinishedState(context, false, Memory(), contracts) :: Nil
    def nextInst = decode(code.binary, instructionPointer)

    def nextStates: Seq[ExecutionState] = {
      val inst = decode(code.binary, instructionPointer)
      inst match {
        case STOP =>
          FinishedState(context, true, Memory(), contracts) :: Nil
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
            case a :: b :: tail =>
              new Keccak256(
                memory.getRange(a, b).values.toSeq,
                memory.getBinary(a, b)
              ) :: tail
          }
        case ADDRESS =>
          simpleInstruction {
            case stack => address :: stack
          }
        case BALANCE =>
          simpleInstruction {
            case a :: tail if contracts contains a => contracts(a).value :: tail
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
            case stack => context.mkTimestamp :: stack
          }
        case NUMBER =>
          simpleInstruction {
            case stack => context.mkBlockNumber :: stack
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
              for ((v, newContext) <- reachableMapValues(a, contract.storage, context)) yield {
                copy(stack = v :: tail, context = newContext).incrementIP
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
            case Constant(a) :: v :: tail  =>
              val validDest = decode(code.binary, a.toInt) == JUMPDEST
              def jumpState(newContext: Context) ={
                if (validDest) copy(instructionPointer = a.toInt, stack = tail, context = newContext)
                else FinishedState(newContext, false, Memory(), contracts)
              }
              context.implies(v) match {
                case Always => jumpState(context) :: Nil
                case Never => copy(stack = tail).incrementIP :: Nil
                case Sometimes(whenYes, whenNo) =>
                  (
                    whenYes.map(newContext => jumpState(newContext))
                      ++
                    whenNo.map(context => copy(stack = tail, context = context).incrementIP)
                  ).toSeq
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
          val decodedValue = code.get(instructionPointer + 1, l)
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
                val code = memory.slice(codeStart, codeLength)
                val newContract = Contract(code, value = value)
                copy(
                  contracts = contracts + (address -> contract.copy(
                    value = contract.value - value,
                    nonce = contract.nonce + 1
                  )) + (newContractAddress -> newContract)
                ).incrementIP
              }
              context.implies(value > contract.value) match {
                case Always =>
                  newContract :: Nil
                case Never =>
                  fail
                case Sometimes(whenYes, whenNo) =>
                  (
                    whenYes.map(context => newContract.copy(context = context))
                      ++
                    whenNo.map(context => FinishedState(context, false, Memory(), contracts))
                  ).toSeq
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
              val enoughMoney = context.implies((value >= 0) & (value <= contracts(address).value))
              val enoughMoneyContexts = enoughMoney match {
                case Always => Set(context)
                case Sometimes(whenYes, _) => whenYes
                case Never => Set.empty
              }
              val callData = memory.slice(dataOffset, dataLength)
              def contractCalls(context: Context): Seq[ExecutionState] = to match {
                case AttackerControlled() =>
                  Seq(
                    attackState(
                      contracts = newContracts,
                      targettedContract = address,
                      context = context,
                      returnDataSize = returnLength,
                      maxCalls = 0
                    ),
                    FinishedState(
                      context = context,
                      success = true,
                      result = attackerContractReturnData(returnLength, context.callCount),
                      contracts = newContracts
                    )
                  )
                case to if contracts contains to =>
                  // Forward to other contract
                  Seq(RunningState(
                    address = to,
                    contracts = newContracts,
                    code = newContracts(address).code,
                    sender = address,
                    callValue = value,
                    callData = callData,
                    callDataLength = dataLength,
                    context = context
                  ))
                case Constant(n) if PrecompiledContracts.Contracts contains n.toInt =>
                  PrecompiledContracts.Contracts(n.toInt)(callData, callDataLength) map {result =>
                    FinishedState(context, result.success, result.data, newContracts)
                  }
                case _ =>
                  // Not a contract, just send money
                  Seq(FinishedState(context, true, Memory(), newContracts))
              }
              (for {
                  newContext <- enoughMoneyContexts.toSeq
                  incrementedContext = newContext.incrementCalls
                  calledState <- contractCalls(incrementedContext)
              } yield {
                ContractCallState(
                  returnState = this.copy(
                    stack = tail
                  ).incrementIP,
                  calledState = calledState,
                  returnLoc = returnOffset,
                  returnSize = returnLength,
                  context = incrementedContext
                )
              }) :+ this.copy(stack = False :: stack).incrementIP
            case _ => fail
          }
        case CALLCODE =>
          stack match {
            case gas :: to :: value :: dataOffset :: dataLength :: returnOffset :: returnLength :: tail =>
              val toContract = contracts.getOrElse(to, Contract(Memory()))
              val callData = memory.slice(dataOffset, dataLength)
              val contractCalls: Seq[ExecutionState] = to match {
                case AttackerControlled() =>
                  // The defender is boned at this point
                  val newContracts = contracts +
                    (address -> contract.copy(value = Constant(0))) +
                    (AttackerControlledAddress -> {
                      val attackerAccount = contracts.getOrElse(AttackerControlledAddress, Contract(Memory()))
                      attackerAccount.copy(value = attackerAccount.value + contract.value)
                    })
                  Seq(FinishedState(
                    context, true, attackerContractReturnData(returnLength, context.callCount),newContracts
                  ))
                case to if contracts contains to =>
                  // Forward to other contract
                  Seq(RunningState(
                    address = address,
                    contracts = contracts,
                    code = toContract.code,
                    sender = address,
                    callValue = value,
                    callData = callData,
                    callDataLength = dataLength,
                    context = context
                  ))
                case Constant(n) if PrecompiledContracts.Contracts contains n.toInt =>
                  PrecompiledContracts.Contracts(n.toInt)(callData, callDataLength) map {result =>
                    FinishedState(context, result.success, result.data, contracts)
                  }
                case _ =>
                  // A weird thing to do, but strictly valid
                  Seq(FinishedState(context, true, Memory(), contracts))
              }
              (for (calledState <- contractCalls) yield {
                ContractCallState(
                  returnState = this.copy(
                    stack = tail
                  ).incrementIP,
                  calledState = calledState,
                  returnLoc = returnOffset,
                  returnSize = returnLength,
                  context = context
                )
              }) :+ this.copy(stack = False :: stack).incrementIP
            case _ => fail
          }
        case DELEGATECALL =>
          stack match {
            case gas :: to :: dataOffset :: dataLength :: returnOffset :: returnLength :: tail =>
              val toContract = contracts.getOrElse(to, Contract(Memory()))
              val callData = memory.slice(dataOffset, dataLength)
              val contractCalls: Seq[ExecutionState] = to match {
                case AttackerControlled() =>
                  // The defender is boned at this point
                  val newContracts = contracts +
                    (address -> contract.copy(value = Constant(0))) +
                    (AttackerControlledAddress -> {
                      val attackerAccount = contracts.getOrElse(AttackerControlledAddress, Contract(Memory()))
                      attackerAccount.copy(value = attackerAccount.value + contract.value)
                    })
                  Seq(FinishedState(
                    context, true, attackerContractReturnData(returnLength, context.callCount), newContracts)
                  )
                case to if contracts contains to =>
                  // Forward to other contract
                  Seq(RunningState(
                    address = address,
                    contracts = contracts,
                    code = toContract.code,
                    sender = sender,
                    callValue = callValue,
                    callData = callData,
                    callDataLength = dataLength,
                    context = context
                  ))
                case Constant(n) if PrecompiledContracts.Contracts contains n.toInt =>
                  PrecompiledContracts.Contracts(n.toInt)(callData, callDataLength) map {result =>
                    FinishedState(context, result.success, result.data, contracts)
                  }
                case _ =>
                  // A weird thing to do, but strictly valid
                  Seq(FinishedState(context, true, Memory(), contracts))
              }
              (for (calledState <- contractCalls) yield {
                ContractCallState(
                  returnState = this.copy(
                    stack = tail
                  ).incrementIP,
                  calledState = calledState,
                  returnLoc = returnOffset,
                  returnSize = returnLength,
                  context = context
                )
              }) :+ this.copy(stack = False :: stack).incrementIP
            case _ => fail
          }
        case RETURN =>
          stack match {
            case a :: b :: tail =>
              Seq(FinishedState(context, true, memory.slice(a, b), contracts))
            case _ => fail
          }
        case UNKNOWN|INVALID|REVERT =>
          fail
        case SELFDESTRUCT =>
          stack match {
            case a :: tail =>
              val recipient = contracts.getOrElse(a, Contract(Memory()))
              Seq(
                FinishedState(context, true, Memory(),
                  contracts - address + (a -> recipient.copy(value = contract.value))),
                FinishedState(context, false, Memory(), contracts) // Yes, SELFDESTRUCT can fail
              )
            case _ => fail
          }
      }
    }
  }
}
