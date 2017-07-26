package io.github.jamespic.ethereum_tools.decompiler.control_flow

import io.github.jamespic.ethereum_tools._
import Bytecode._
/**
 * A relatively data flow model for control flow analysis - we want to
 * distinguish jump dests that are either constant, passed on stack, or calculated.
 */
case class StackState(vars: List[Expr] = Nil, thenIndex: Int = 0) {
  def pop: (Expr, StackState) = vars match {
    case head :: tail => (head, copy(vars = tail))
    case Nil => (StackVar(thenIndex), copy(thenIndex = thenIndex + 1))
  }
  def pop(n: Int): (List[Expr], StackState) = {
    var (vars, newStack) = (0 until n).foldLeft((List.empty[Expr], this)){
      case ((vars, stack), i) =>
        val (e, newStack) = stack.pop
        (e :: vars, newStack)
    }
    (vars.reverse, newStack)
  }
  def push(exprs: Expr*): StackState = {
    (exprs :\ this)((exp, s) => s.copy(vars = exp :: s.vars))
  }
  def progress(op: Bytecode) = op match {
    case _ => ???
  }
  lazy val height = vars.length - thenIndex
}

sealed trait Expr
case class StackVar(index: Int) extends Expr
case class ConstExpr(const: BigInt) extends Expr
case object CalculatedExpr extends Expr
