package io.github.jamespic.ethereum_tools.decompiler.control_flow

import scala.collection.immutable.SortedSet

import io.github.jamespic.ethereum_tools._
import Bytecode._

object Block {
  def identifyBasicBlocks(instructions: InstList): SortedSet[BasicBlock] = {
    val result = SortedSet.newBuilder[BasicBlock]
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
}

sealed trait Block {
  val address: Int
  def exitPoint: ExitPoint
}

object BasicBlock {
  implicit val ordering: Ordering[BasicBlock] = Ordering.by(_.address)
}

case class BasicBlock(address: Int, instructions: InstList, stackHeightDelta: Int, exitPoint: ExitPoint) extends Block {
  override def toString =
    f"Block ${address}%04x {\n" +
      instructions.map(i => f"    ${i._1}%04x ${i._2}%s").mkString("\n") +
    "\n} -> " + exitPoint + "\n\n"
}
