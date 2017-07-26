package io.github.jamespic.ethereum_tools.decompiler
import io.github.jamespic.ethereum_tools._


object Block {
  def makeBlocks(instructions: List[(Int, Bytecode)]): List[(Int, Block)] = {
    val blocks = List.newBuilder[(Int, Block)]
    var statements = List.newBuilder[Stmt]
    var stack = StackState()
    var blockStart = 0
    var namedId = 0
    for ((i, opcode) <- instructions) {
      def flushBlock(block: Block) {
        blocks += blockStart -> block
        statements = List.newBuilder[Stmt]
        stack = StackState()
        blockStart = i + 1
      }

      opcode match {
        case STOP =>
          statements += StopStmt
          flushBlock(StopBlock(stack, statements.result()))
        case ADDRESS|ORIGIN|CALLER|CALLVALUE|CALLDATASIZE =>
          stack = stack.push(NonExpr(opcode))
        case ISZERO|NOT|CALLDATALOAD =>
          val (e, stack1) = stack.pop
          stack = stack1.push(UnarExpr(opcode, e))
        case ADD|MUL|SUB|DIV|SDIV|MOD|SMOD|EXP|SIGNEXTEND|LT|GT|SLT|SGT|EQ
             |AND|OR|XOR|BYTE =>
          val (e1, stack1) = stack.pop
          val (e2, stack2) = stack2.pop
          stack = stack2.push(BinExpr(opcode, e1, e2))
        case ADDMOD|MULMOD =>
          val (e1, stack1) = stack.pop
          val (e2, stack2) = stack2.pop
          val (e3, stack3) = stack3.pop
          stack = stack3.push(TertExpr(opcode, e1, e2, e3))
        case SHA3|BALANCE =>
          val invars = for (i <- (0 until opcode.inputs)) yield {
            val (e, newStack) = stack.pop
            stack = newStack
            e
          }
          namedId += 1
          val newVar = NamedVar(namedId)
          stack = stack.push(newVar)
          statements += SaveResultStmt(opcode, invars.toList)

      }
    }
    statements += StopStmt
    blocks += blockStart -> StopBlock(stack, statements.result())
    blocks.result()
  }
}

case class StackState(vars: List[Expr] = Nil, thenIndex: Int = 0) {
  def pop: (Expr, StackState) = vars match {
    case head :: tail => (head, StackState(tail, thenIndex))
    case Nil => (StackVar(thenIndex), StackState(Nil, thenIndex + 1))
  }
  def push(expr: Expr) = StackState(expr :: vars, thenIndex)
}

sealed trait Block {
  val stack: StackState
  val statements: List[Stmt]
}
case class StopBlock(stack: StackState, statements: List[Stmt]) extends Block

sealed trait Stmt
case object StopStmt extends Stmt
case class SaveResultStmt(op: Bytecode, inputs: List[Expr]) extends Stmt

sealed trait Expr
case class StackVar(index: Int) extends Expr
case class NamedVar(num: Int) extends Expr
case class NonExpr(op: Bytecode) extends Expr
case class UnarExpr(op: Bytecode, e1: Expr) extends Expr
case class BinExpr(op: Bytecode, e1: Expr, e2: Expr) extends Expr
case class TertExpr(op: Bytecode, e1: Expr, e2: Expr, e3) extends Expr
case class OrderedExpr(op: Bytecode, inputs: List[Expr], outputs: List[Expr])

// FIXME: Resume at CALLDATACOPY
