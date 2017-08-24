package io.github.jamespic.ethereum_tools.decompiler.data_flow
import io.github.jamespic.ethereum_tools.decompiler.control_flow.{ConstExpr => CfConst, _}
import io.github.jamespic.ethereum_tools._
import Bytecode._

sealed trait Stmt
sealed trait Expr {
  def dirty: Boolean
}
sealed trait LValue extends Expr
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

case class VarExpr(n: Int) extends StrRepr(s"var_$n") with CleanExpr with LValue
case object OkVarExpr extends StrRepr("ok") with DirtyExpr with LValue
case object NewContractVar extends StrRepr("newContract") with DirtyExpr with LValue
case class ArgExpr(n: Int) extends StrRepr(s"arg_$n") with CleanExpr with LValue
case object ReturnLocationExpr extends StrRepr("__RETURN_POINTER__") with CleanExpr
case class SetStmt(varExp: LValue, value: Expr) extends StrRepr(s"$varExp = $value;") with Stmt
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
case class EqExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a == $b)", a, b)
case class IszeroExpr(a: Expr) extends MaybeDirtyExpr(s"($a == 0)", a)
case class AndExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a & $b)", a, b)
case class OrExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a | $b)", a, b)
case class XorExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a ^ $b)", a, b)
case class NotExpr(a: Expr) extends MaybeDirtyExpr(s"~$a", a)
case class ByteExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"bytes32($a)[$b]")
case class SHA3Expr(a:Expr, b: Expr) extends StrRepr(s"sha3(MEMORY[$a..($b + $a)])") with DirtyExpr
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
case class JumpDestStmt(i: Int) extends StrRepr(f"TAG_$i%x:") with Stmt
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
case object ThrowStmt extends StrRepr("throw;") with Stmt
case class RevertStmt(a: Expr, b: Expr) extends StrRepr("revert();") with Stmt
case class SelfdestructStmt(a: Expr) extends StrRepr(s"selfdestruct($a);") with Stmt

case class GotoStmt(a: Expr) extends StrRepr(s"GOTO ($a);") with Stmt
case class IfStmt(cond: Expr, block: Stmt) extends StrRepr(s"if ($cond) $block") with Stmt
case class FunctionReturnStmt(exprs: Expr*) extends Stmt {
  override def toString =
    if (exprs.length > 0) s"return (${exprs.mkString(", ")});"
    else "return;"
}
case class FunctionCallStmt(name: String, inputs: List[Expr], outputs: List[LValue]) extends Stmt {
  def this(expr: Expr, inputs: List[Expr], outputs: List[LValue]) = {
    this(expr match {
      case ConstExpr(n) => f"func_$n%x"
      case e => s"($e)"
    }, inputs, outputs)
  }
  override def toString = s"(${outputs.mkString(", ")}) = $name(${inputs.mkString(", ")})"
}

case class FunctionAST(name: String, inputs: Int, outputs: Int, stmts: List[Stmt]) {
  override def toString = {
    val vars = ((0 until inputs) map ArgExpr).mkString(", ")
    val returns = Seq.fill(outputs)("int").mkString(", ")
    val code = stmts.map("  " + _).mkString("\n  ")
    s"function $name($vars) returns ($returns) {\n  $code\n}\n\n"
  }
}


object AST {
  def funcsToAst(funcs: Set[Func], signatureHints: Set[SignatureHint]) = {
    for (Func(address, code, inputs, outputs, startingStates) <- funcs) yield {
      val startStacks = for ((addr, startStack) <- startingStates) yield {
        val normStack = startStack.ensureDepth(startStack.height + inputs + 1)
        addr -> (normStack.vars.zipWithIndex map {
          case (StackVar(`inputs`), _) => ReturnLocationExpr
          case (StackVar(n), _) => ArgExpr(inputs - n - 1)
          case (CfConst(n), _) => ConstExpr(n)
          case (CalculatedExpr, i) => VarExpr(normStack.vars.size - i)
        })
      }
      FunctionAST(f"func_$address%x", inputs, outputs,
        for (BasicBlock(addr, instructions, stateChange) <- code.blocks.toList) yield {
          val startStack = startStacks.getOrElse(addr, Nil)
          var stmts = List.newBuilder[Stmt]
          var exprStack: List[Expr] = startStack
          var highestVarNum = (for (VarExpr(n) <- exprStack) yield n).fold(0)(_ max _)
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

          def jumpStmtList(nextLoc: Expr): List[Stmt] = {
            nextLoc match {
              case ReturnLocationExpr => List(FunctionReturnStmt(exprStack: _*))
              case ConstExpr(n) if startStacks contains n.toInt =>
                // Local jump
                val blockEntryStack = startStacks(n.toInt)
                List(
                  SetStmtList(for {
                    (expected, actual) <- blockEntryStack zip exprStack
                    if expected != actual
                  } yield SetStmt(expected.asInstanceOf[LValue], actual)),
                  GotoStmt(ConstExpr(n))
                )
              case ConstExpr(n) if funcs.exists(_.address == n.toInt) =>
                val func = funcs.find(_.address == n.toInt).get
                val inputExprs = List.fill(func.inputs)(pop())
                val outputExprs = List.fill(func.outputs)(newVar())
                val returnExpr = pop()
                flushStack()
//                exprStack = outputExprs ::: exprStack
                List(new FunctionCallStmt(ConstExpr(func.address), inputExprs, outputExprs)) ++ jumpStmtList(returnExpr)
              case x =>
                signatureHints collectFirst {
                  case SignatureHint(`address`, funcInputs, funcOutputs, returnAddr) =>
                    val inputExprs = List.fill(funcInputs)(pop())
                    val outputExprs = List.fill(funcOutputs)(newVar())
                    flushStack()
//                    exprStack = outputExprs ::: exprStack
                    List(new FunctionCallStmt(x, inputExprs, outputExprs)) ++ jumpStmtList(ConstExpr(returnAddr))
                } getOrElse {
                  // Wild GOTO
                  List(GotoStmt(x))
                }
            }
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
              case EQ => push(EqExpr(pop(), pop()))
              case ISZERO => push(IszeroExpr(pop()))
              case AND => push(AndExpr(pop(), pop()))
              case OR => push(OrExpr(pop(), pop()))
              case XOR => push(XorExpr(pop(), pop()))
              case NOT => push(NotExpr(pop()))
              case BYTE => push(ByteExpr(pop(), pop()))
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
                stmts ++= jumpStmtList(pop())
              case JUMPI =>
                val loc = pop()
                val cond = pop()
                stmts += IfStmt(cond, StmtList(jumpStmtList(loc)))
              case PC => push(PcExpr)
              case MSIZE => push(MsizeExpr)
              case GAS => push(GasExpr)
              case JUMPDEST =>
                assert(exprStack.forall(!_.dirty))
                stmts += JumpDestStmt(i)
              case PUSH(_, x) => push(ConstExpr(x))
              case DUP(n) => push(exprStack(n - 1))
              case SWAP(n) =>
                val (a :: head, b :: tail) = exprStack.splitAt(n)
                exprStack = b :: head ::: a :: tail
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
              case INVALID|UNKNOWN => stmts += ThrowStmt
              case REVERT => stmts += RevertStmt(pop(), pop())
              case SUICIDE => stmts += SelfdestructStmt(pop())
            }
          }

          // Optionally prepare for fallthrough
          (stateChange.exitPoint match {
            case Fallthrough(n) => Some(n)
            case ConditionalExit(_, Fallthrough(n)) => Some(n)
            case _ => None
          }) foreach {n =>
            if (startStacks contains n) {
              // FIXME: Deal with damn-weird corner case when fallthrough has inconsistent stack height
              stmts += SetStmtList(for {
                (expected, actual) <- startStacks(n) zip exprStack
                if expected != actual
              } yield SetStmt(expected.asInstanceOf[LValue], actual))
            }
          }

          StmtList(stmts.result())
        }
      )
    }
  }
}
