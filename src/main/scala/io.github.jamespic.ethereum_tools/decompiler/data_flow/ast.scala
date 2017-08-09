package io.github.jamespic.ethereum_tools.decompiler.data_flow
import io.github.jamespic.ethereum_tools.decompiler.control_flow.BasicBlock

sealed trait Stmt
sealed trait Expr
abstract class StrRepr(repr: String) {
  override def toString = repr
}

case class VarExpr(n: Int) extends StrRepr(s"var_$n") with Expr
case class SetStmt(varExp: VarExpr, value: Expr) extends StrRepr(s"$varExp = $value;")
case class StmtList(stmts: List[Stmt]) extends StrRepr(stmts.mkString("\n")) with Stmt

case object StopStmt extends StrRepr("return;") with Stmt

object AST {
  def blockToStmtList(block: BasicBlock, startStackHeight: Int) = {
    var stmts = List.newBuilder[Stmt]
    var exprStack: List[Expr] = (for (i <- 0 until startStackHeight) yield VarExpr(i)).toList
    def flushStack() = {
      for ((i, expr) <- exprStack.)
    }
    for ((_, inst) <- block.instructions) {
      inst match {

      }
    }

  }
}
