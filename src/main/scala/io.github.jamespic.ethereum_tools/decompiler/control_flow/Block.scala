package io.github.jamespic.ethereum_tools.decompiler.control_flow

import scala.collection.immutable.SortedSet

import io.github.jamespic.ethereum_tools._
import Bytecode._

sealed trait Block {
  val address: Int
  def exitPoint: ExitPoint
}

case class BasicBlock(address: Int, instructions: InstList, stackHeightDelta: Int, exitPoint: ExitPoint) extends Block {
  override def toString =
    f"Block ${address}%04x {\n" +
      instructions.map(i => f"    ${i._1}%04x ${i._2}%s").mkString("\n") +
    "\n} -> " + exitPoint + "\n\n"
}

case class FunctionBlock(address: Int, code: Block, inputs: Int, outputs: Int) extends Block {
  assert(code.exitPoint match {
    case FunctionReturn(`inputs`) |
      WithEarlyFunctionReturn(`inputs`, Throw|FunctionReturn(`inputs`)) => true
    case _ => false
  })

  override def toString =
    f"function ${address}%04x (${inputs}%d inputs, ${outputs}%d outputs) {\n" +
      code.toString.split("\n").map("    " + _).mkString("\n") +
    "\n} -> " + exitPoint + "\n\n"

  override def exitPoint: ExitPoint = code.exitPoint
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
        result += BasicBlock(blockStart, block, currentStack.height, exitPoint)
      }
      currentBlock = List.newBuilder[(Int, Bytecode)]
      currentStack = StackState()
      blockStart = -1

    }
    for ((i, op) <- instructions) {
      if (op == JUMPDEST) finishBlock(ConstJump(i))

      var (stackHead, _) = currentStack.pop
      if (blockStart == -1) blockStart = i
      currentBlock += i -> op
      currentStack = currentStack.progress(op)

      (op, stackHead) match {
        case (STOP|RETURN, _) =>
          finishBlock(Halt)
        case (INVALID|REVERT|SUICIDE|UNKNOWN, _) =>
          finishBlock(Throw)
        case (JUMP, StackVar(n)) =>
          finishBlock(FunctionReturn(n))
        case (JUMP, ConstExpr(n)) =>
          finishBlock(ConstJump(n.toInt))
        case (JUMP, CalculatedExpr) =>
          finishBlock(CalculatedJump)
        case (JUMPI, StackVar(n)) =>
          finishBlock(ConditionalExit(FunctionReturn(n), ConstJump(i + op.opcodeSize)))
        case (JUMPI, ConstExpr(n)) =>
          finishBlock(ConditionalExit(ConstJump(n.toInt), ConstJump(i + op.opcodeSize)))
        case (JUMPI, CalculatedExpr) =>
          finishBlock(ConditionalExit(CalculatedJump, ConstJump(i + op.opcodeSize)))
        case _ => // Continue
      }
    }
    finishBlock(Halt)
    result.result()
  }

  implicit def ordering[T <: Block]: Ordering[T] = Ordering.by(_.address)
}
