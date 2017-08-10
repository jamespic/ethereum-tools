package io.github.jamespic.ethereum_tools.decompiler.data_flow
import io.github.jamespic.ethereum_tools._
import Bytecode._

sealed trait Stmt
sealed trait Expr {
  def dirty: Boolean
}
sealed trait CleanExpr extends Expr {
  override def dirty = false
}
sealed trait DirtyExpr extends Expr {
  override def dirty = true
}
abstract class StrRepr(repr: String) {
  override def toString = repr
}
abstract class MaybeDirtyExpr(repr: String, elements: Expr*) extends Expr {
  override def toString = repr
  override def dirty = elements.exists(_.dirty)
}

case class VarExpr(n: Int) extends StrRepr(s"var_$n") with CleanExpr
case object OkVarExpr extends StrRepr("ok") with DirtyExpr
case class ArgExpr(n: Int) extends StrRepr(s"arg_$n") with CleanExpr
case object ReturnLocationExpr extends CleanExpr
case class SetStmt(varExp: VarExpr, value: Expr) extends StrRepr(s"$varExp = $value;") with Stmt
case class SetStmtList(sets: Seq[SetStmt]) extends Stmt {
  override def toString = sets.map(_.varExp).mkString("(", ", ", ")") + " = " + sets.map(_.value).mkString("(", ", ", ")") + ";"
}
case class StmtList(stmts: List[Stmt]) extends StrRepr(stmts.mkString("{\n  ", "\n  ", "}")) with Stmt

case object StopStmt extends StrRepr("return;") with Stmt
case class AddExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a + $b)", a, b)
case class MulExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a * $b)", a, b)
case class SubExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a - $b)", a, b)
case class DivExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a / $b)", a, b)
case class SdivExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a /$$ $b)", a, b)
case class ModExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a % $b)", a, b)
case class SmodExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a %$$ $b)", a, b)
case class AddmodExpr(a: Expr, b: Expr, c: Expr) extends MaybeDirtyExpr(s"addmod($a, $b, $c)", a, b, c)
case class MulmodExpr(a: Expr, b: Expr, c: Expr) extends MaybeDirtyExpr(s"mulmod($a, $b, $c)", a, b, c)
case class ExpExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a ** $b)", a, b)
case class SignExtendExpr(a: Expr) extends MaybeDirtyExpr(s"SIGNEXTEND($a)", a)
case class LtExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a < $b)", a, b)
case class GtExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a > $b)", a, b)
case class SltExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a <$$ $b)", a, b)
case class SgtExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a >$$ $b)", a, b)
case class IszeroExpr(a: Expr) extends MaybeDirtyExpr(s"($a != 0)", a)
case class AndExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a & $b)", a, b)
case class OrExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a | $b)", a, b)
case class XorExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a ^ $b)", a, b)
case class NotExpr(a: Expr) extends MaybeDirtyExpr(s"~$a", a)
case class ByteExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"bytes32($a)[$b]")
case class SHA3Expr(a:Expr, b: Expr) extends StrRepr(s"sha3(MEMORY[$a..($b - $a)])") with DirtyExpr
case object AddressExpr extends StrRepr("this") with CleanExpr
case class BalanceExpr(a: Expr) extends StrRepr(s"$a.balance") with DirtyExpr
case object OriginExpr extends StrRepr("tx.origin") with CleanExpr
case object CallerExpr extends StrRepr("msg.sender") with CleanExpr
case object CallvalueExpr extends StrRepr("msg.value") with CleanExpr
case class CalldataloadExpr(a: Expr) extends StrRepr(s"msg.DATA[$a..$a+32]") with CleanExpr
case object CalldatasizeExpr extends StrRepr("msg.data.length") with CleanExpr
case class CalldatacopyStmt(a: Expr, b: Expr, c: Expr) extends StrRepr(s"MEMORY[$a..($a + $c)] = msg.DATA[$b..($b +$c)];") with Stmt
case object CodesizeExpr extends StrRepr("this.CODE.length") with CleanExpr
case class CodecopyStmt(a: Expr, b: Expr, c: Expr) extends StrRepr(s"MEMORY[$a..($a + $c)] = this.CODE[$b..($b +$c)];") with Stmt
case object GasPriceExpr extends StrRepr("tx.gasprice") with CleanExpr
case class ExtcodesizeExpr(a: Expr) extends StrRepr(s"$a.CODE.length") with DirtyExpr
case class ExtcodecopyStmt(a: Expr, b: Expr, c: Expr, d: Expr) extends StrRepr(s"MEMORY[$b..($b + $d)] = $a.CODE[$c..($d +$d)];") with Stmt
case class BlockhashExpr(a: Expr) extends StrRepr(s"block.blockhash($a)") with CleanExpr
case object CoinbaseExpr extends StrRepr("block.coinbase") with CleanExpr
case object TimestampExpr extends StrRepr("block.timestamp") with CleanExpr
case object NumberExpr extends StrRepr("block.number") with CleanExpr
case object DifficultyExpr extends StrRepr("block.difficulty") with CleanExpr
case object GaslimitExpr extends StrRepr("block.gaslimit") with CleanExpr
case class MloadExpr(a: Expr) extends StrRepr(s"MEMORY[$a..($a + 32)]") with DirtyExpr
case class MstoreStmt(a: Expr, b: Expr) extends StrRepr(s"MEMORY[$a..($a + 32)] = $b;") with Stmt
case class Mstore8Stmt(a: Expr, b: Expr) extends StrRepr(s"MEMORY[$a] = int8($b);") with Stmt
case class SloadExpr(a: Expr) extends StrRepr(s"STORAGE[$a]") with DirtyExpr
case class SstoreStmt(a: Expr, b: Expr) extends StrRepr(s"STORAGE[$a] = $b;") with Stmt
case object PcExpr extends StrRepr("__PROGRAM_COUNTER__") with CleanExpr
case object MsizeExpr extends StrRepr("MEMORY.length") with DirtyExpr
case object GasExpr extends StrRepr("msg.gas") with DirtyExpr
case class JumpDestStmt(i: Int) extends StrRepr(s"TAG_$i:") with Stmt
case class ConstExpr(a: BigInt) extends StrRepr(f"0x$a%x") with CleanExpr
case class LogStmt(a: Expr, b: Expr, topics: Expr*) extends StrRepr(s"LOG(${topics.mkString(", ")}, MEMORY[$a..($a + $b)])") with Stmt
case class CreateStmt(returnVar: VarExpr, a: Expr, b: Expr, c: Expr) extends Stmt {
  override def toString = a match {
    case ConstExpr(n) if n == BigInt(0) => s"$returnVar = new MEMORY[$b..($b + $c)]();"
    case _ => s"$returnVar = new MEMORY[$b..($b + $c)].value($a)();"
  }
}
case class CallStmt(
  gas: Expr, addr: Expr, value: Expr,
  inOffset: Expr, inLength: Expr, outOffset: Expr, outLength: Expr
) extends Stmt {
  override def toString = {
    val valueInfo = value match {
      case ConstExpr(n) if n == BigInt(0) => ""
      case _ => s".value($value)"
    }
    s"(ok, MEMORY[$outOffset..($outOffset + $outLength)]) = $addr.call.gas($gas)$valueInfo(MEMORY[$inOffset..($inOffset + $inLength)]);"
  }
}
case class CallcodeStmt(
  gas: Expr, addr: Expr, value: Expr,
  inOffset: Expr, inLength: Expr, outOffset: Expr, outLength: Expr
) extends Stmt {
  override def toString = {
    val valueInfo = value match {
      case ConstExpr(n) if n == BigInt(0) => ""
      case _ => s".value($value)"
    }
    s"(ok, MEMORY[$outOffset..($outOffset + $outLength)]) = $addr.callcode.gas($gas)$valueInfo(MEMORY[$inOffset..($inOffset + $inLength)]);"
  }
}
case class ReturnStmt(a: Expr, b: Expr) extends StrRepr(s"return MEMORY[$a..($a + $b)];") with Stmt
case class DelegatecallStmt(
  gas: Expr, addr: Expr,
  inOffset: Expr, inLength: Expr, outOffset: Expr, outLength: Expr
) extends Stmt {
  override def toString = {
    s"(ok, MEMORY[$outOffset..($outOffset + $outLength)]) = $addr.delegatecall.gas($gas)(MEMORY[$inOffset..($inOffset + $inLength)]);"
  }
}
case class ThrowStmt(a: Expr, b: Expr) extends StrRepr("throw;") with Stmt
case class SelfdestructStmt(a: Expr) extends StrRepr(s"selfdestruct($a);") with Stmt

case class GotoStmt(a: Expr) extends StrRepr(s"GOTO ($a);") with Stmt
case class IfStmt(cond: Expr, block: Stmt) extends StrRepr(s"if ($cond) $block") with Stmt
case class FunctionReturnStmt(exprs: Expr*) extends Stmt {
  override def toString =
    if (exprs.length > 0) s"return (${exprs.mkString(", ")});"
    else "return;"
}


object AST {
  def blockToStmtList(instructions: InstList, startStackHeight: Int): StmtList = {
    blockToStmtList(instructions, (for (i <- 0 until startStackHeight) yield VarExpr(i)).toList)
  }
  def blockToStmtList(instructions: InstList, startStack: List[Expr]): StmtList = {
    var stmts = List.newBuilder[Stmt]
    var exprStack = startStack
    var highestVarNum = (for (VarExpr(n) <- exprStack) yield n).max
    def newVar() = {
      highestVarNum += 1
      VarExpr(highestVarNum)
    }
    def pop() = {
      val head :: tail = exprStack
      exprStack = tail
      head
    }
    def push(expr: Expr) = exprStack = expr :: exprStack
    def flushStack() = {
      exprStack = for (expr <- exprStack) yield {
        if (expr.dirty) {
          val varExpr = newVar()
          stmts += SetStmt(varExpr, expr)
          varExpr
        } else {
          expr
        }
      }
    }

    def resetStackStmt() = {
      SetStmtList(for ((expr, i) <- exprStack.zipWithIndex) yield SetStmt(VarExpr(i), expr))
    }

    for ((i, inst) <- instructions) {
      inst match {
        case STOP => stmts += StopStmt
        case ADD => push(AddExpr(pop(), pop()))
        case MUL => push(MulExpr(pop(), pop()))
        case SUB => push(SubExpr(pop(), pop()))
        case DIV => push(DivExpr(pop(), pop()))
        case SDIV => push(SdivExpr(pop(), pop()))
        case MOD => push(ModExpr(pop(), pop()))
        case SMOD => push(SmodExpr(pop(), pop()))
        case ADDMOD => push(AddmodExpr(pop(), pop(), pop()))
        case MULMOD => push(MulmodExpr(pop(), pop(), pop()))
        case EXP => push(ExpExpr(pop(), pop()))
        case SIGNEXTEND => push(SignExtendExpr(pop()))
        case LT => push(LtExpr(pop(), pop()))
        case GT => push(GtExpr(pop(), pop()))
        case SLT => push(SltExpr(pop(), pop()))
        case SGT => push(SgtExpr(pop(), pop()))
        case ISZERO => push(IszeroExpr(pop()))
        case AND => push(AndExpr(pop(), pop()))
        case OR => push(OrExpr(pop(), pop()))
        case XOR => push(XorExpr(pop(), pop()))
        case NOT => push(NotExpr(pop()))
        case SHA3 => push(SHA3Expr(pop(), pop()))
        case ADDRESS => push(AddressExpr)
        case BALANCE => push(BalanceExpr(pop()))
        case ORIGIN => push(OriginExpr)
        case CALLER => push(CallerExpr)
        case CALLVALUE => push(CallvalueExpr)
        case CALLDATALOAD => push(CalldataloadExpr(pop()))
        case CALLDATASIZE => push(CalldatasizeExpr)
        case CALLDATACOPY =>
          val a = pop()
          val b = pop()
          val c = pop()
          flushStack()
          stmts += CalldatacopyStmt(a, b, c)
        case CODESIZE => push(CodesizeExpr)
        case CODECOPY =>
          val a = pop()
          val b = pop()
          val c = pop()
          flushStack()
          stmts += CodecopyStmt(a, b, c)
        case GASPRICE => push(GasPriceExpr)
        case EXTCODESIZE => push(ExtcodesizeExpr(pop()))
        case EXTCODECOPY =>
          val a = pop()
          val b = pop()
          val c = pop()
          val d = pop()
          flushStack()
          stmts += ExtcodecopyStmt(a, b, c, d)
        case BLOCKHASH => push(BlockhashExpr(pop()))
        case COINBASE => push(CoinbaseExpr)
        case TIMESTAMP => push(TimestampExpr)
        case NUMBER => push(NumberExpr)
        case DIFFICULTY => push(DifficultyExpr)
        case GASLIMIT => push(GaslimitExpr)
        case POP => pop()
        case MLOAD => push(MloadExpr(pop()))
        case MSTORE =>
          val a = pop()
          val b = pop()
          flushStack()
          stmts += MstoreStmt(a, b)
        case MSTORE8 =>
          val a = pop()
          val b = pop()
          flushStack()
          stmts += Mstore8Stmt(a, b)
        case SLOAD => push(SloadExpr(pop()))
        case SSTORE =>
          val a = pop()
          val b = pop()
          flushStack()
          stmts += SstoreStmt(a, b)
        case JUMP =>
          pop() match {
            case ReturnLocationExpr =>
              stmts += FunctionReturnStmt(exprStack: _*)
            case a =>
              stmts += resetStackStmt()
              stmts += GotoStmt(a)
          }
        case JUMPI =>
          val loc = pop()
          val cond = pop()
          loc match {
            case ReturnLocationExpr =>
              stmts += IfStmt(cond, FunctionReturnStmt(exprStack: _*))
            case _ =>
              stmts += IfStmt(cond, StmtList(List(
                resetStackStmt(),
                GotoStmt(loc)
              )))
          }
        case PC => push(PcExpr)
        case MSIZE => push(MsizeExpr)
        case GAS => push(GasExpr)
        case JUMPDEST =>
          assert(exprStack.forall(!_.dirty))
          stmts += JumpDestStmt(i)
        case PUSH(_, x) => push(ConstExpr(x))
        case DUP(n) => push(exprStack(n - 1))
        case SWAP(n) =>
          val (head, tail) = exprStack.splitAt(n + 1)
          exprStack = head.last :: head.dropRight(1) ::: tail
        case LOG(n) =>
          val a = pop()
          val b = pop()
          val topics = Seq.fill(n)(pop())
          flushStack()
          stmts += LogStmt(a, b, topics: _*)
        case CREATE =>
          val a = pop()
          val b = pop()
          val c = pop()
          val returnVar = newVar()
          flushStack()
          stmts += CreateStmt(returnVar, a, b, c)
        case CALL =>
          val gas = pop()
          val addr = pop()
          val value = pop()
          val inOffset = pop()
          val inLength = pop()
          val outOffset = pop()
          val outLength = pop()
          flushStack()
          stmts += CallStmt(gas, addr, value, inOffset, inLength, outOffset, outLength)
          push(OkVarExpr)
        case CALLCODE =>
          val gas = pop()
          val addr = pop()
          val value = pop()
          val inOffset = pop()
          val inLength = pop()
          val outOffset = pop()
          val outLength = pop()
          flushStack()
          stmts += CallcodeStmt(gas, addr, value, inOffset, inLength, outOffset, outLength)
          push(OkVarExpr)
        case RETURN =>
          stmts += ReturnStmt(pop(), pop())
        case DELEGATECALL =>
          val gas = pop()
          val addr = pop()
          val inOffset = pop()
          val inLength = pop()
          val outOffset = pop()
          val outLength = pop()
          flushStack()
          stmts += DelegatecallStmt(gas, addr, inOffset, inLength, outOffset, outLength)
          push(OkVarExpr)
        case INVALID|UNKNOWN => stmts += ThrowStmt(pop(), pop())
        case SUICIDE => stmts += SelfdestructStmt(pop())
      }
    }
    StmtList(stmts.result())
  }
}
