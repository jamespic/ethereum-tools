package io.github.jamespic.ethereum_tools.decompiler.control_flow

import scala.collection.immutable.SortedSet

import io.github.jamespic.ethereum_tools._
import Bytecode._

sealed trait Block {
  val address: Int
  def stateChange: StateChange
  def exitPoint: ExitPoint = stateChange.exitPoint
  def stackChange: StackState = stateChange.stackState
}

case class BasicBlock(address: Int, instructions: InstList, stateChange: StateChange) extends Block {
  override def toString =
    f"Block ${address}%04x {\n" +
      instructions.map(i => f"    ${i._1}%04x ${i._2}%s").mkString("\n") +
    "\n} -> " + exitPoint + "\n\n"
}

case class IfBlock(address: Int, decisionBlock: Block, trueBlock: Block, stateChange: StateChange) extends Block {
  override def toString =
    f"if ${address}%04x  {\n" +
      Block.printIndented(decisionBlock)
    "\n} {\n" +
      Block.printIndented(trueBlock)
    "\n} -> " + exitPoint + "\n\n"
}

case class UnlessBlock(address: Int, decisionBlock: Block, falseBlock: Block, stateChange: StateChange) extends Block {
  override def toString =
    f"unless ${address}%04x  {\n" +
      Block.printIndented(decisionBlock)
    "\n} {\n" +
      Block.printIndented(falseBlock)
    "\n} -> " + exitPoint + "\n\n"
}

case class PassThroughBlock(address: Int, block1: Block, block2: Block, stateChange: StateChange) extends Block {
  override def toString =
    f"passthrough ${address}%04x  {\n" +
      Block.printIndented(block1)
    "\n} {\n" +
      Block.printIndented(block2)
    "\n} -> " + exitPoint + "\n\n"
}

case class FunctionBlock(address: Int, code: Block, inputs: Int, outputs: Int) extends Block {
  assert(code.exitPoint match {
    case StackJump(`inputs`)=> true
    case _ => false
  })
  assert(code.stackChange.height == outputs - inputs)

  override def toString =
    f"function ${address}%04x (${inputs}%d inputs, ${outputs}%d outputs) {\n" +
      code.toString.split("\n").map("    " + _).mkString("\n") +
    "\n} -> " + exitPoint + "\n\n"

  override def stateChange = StateChange(StackJump(inputs), code.stackChange)
}

object Block {
  def identifyBasicBlocks(instructions: InstList): SortedSet[Block] = {
    val result = SortedSet.newBuilder[Block]
    var currentBlock = List.newBuilder[(Int, Bytecode)]
    var currentStack = StackState()
    var blockStart = -1
    def finishBlock(exitPoint: ExitPoint) = {
      var block = currentBlock.result()
      if (block.nonEmpty) {
        result += BasicBlock(blockStart, block, StateChange(exitPoint, currentStack))
      }
      currentBlock = List.newBuilder[(Int, Bytecode)]
      currentStack = StackState()
      blockStart = -1

    }
    for ((i, op) <- instructions) {
      if (op == JUMPDEST) finishBlock(Fallthrough(i))

      var (stackHead, _) = currentStack.pop
      if (blockStart == -1) blockStart = i
      currentBlock += i -> op
      currentStack = currentStack.progress(op)

      (op, stackHead) match {
        case (STOP|RETURN, _) =>
          finishBlock(Halt)
        case (INVALID | REVERT | SELFDESTRUCT | UNKNOWN, _) =>
          finishBlock(Throw)
        case (JUMP, StackVar(n)) =>
          finishBlock(StackJump(n))
        case (JUMP, ConstExpr(n)) =>
          finishBlock(ConstJump(n.toInt))
        case (JUMP, CalculatedExpr) =>
          finishBlock(CalculatedJump)
        case (JUMPI, StackVar(n)) =>
          finishBlock(ConditionalExit(StackJump(n), Fallthrough(i + op.opcodeSize)))
        case (JUMPI, ConstExpr(n)) =>
          finishBlock(ConditionalExit(ConstJump(n.toInt), Fallthrough(i + op.opcodeSize)))
        case (JUMPI, CalculatedExpr) =>
          finishBlock(ConditionalExit(CalculatedJump, ConstJump(i + op.opcodeSize)))
        case _ => // Continue
      }
    }
    finishBlock(Halt)
    result.result()
  }

  implicit def ordering[T <: Block]: Ordering[T] = Ordering.by(_.address)
  private[control_flow] def printIndented(code: Any) = {
    code.toString.split("\n").map("    " + _).mkString("\n")
  }

  def unapply(block: Block) = Some((block.address, block.stateChange))
}
