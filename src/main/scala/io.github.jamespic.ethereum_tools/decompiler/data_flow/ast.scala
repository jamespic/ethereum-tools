package io.github.jamespic.ethereum_tools.decompiler.data_flow
import io.github.jamespic.ethereum_tools.decompiler.control_flow.{ConstExpr => CfConst, _}
import io.github.jamespic.ethereum_tools._
import Bytecode._

sealed trait Stmt
sealed trait Expr {
  def dirty: Boolean
}
sealed trait BoolExpr extends Expr
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
case object OkVarExpr extends StrRepr("ok") with DirtyExpr with LValue with BoolExpr
case object NewContractVar extends StrRepr("newContract") with DirtyExpr with LValue
case class ArgExpr(n: Int) extends StrRepr(s"arg_$n") with CleanExpr with LValue
case object ReturnLocationExpr extends StrRepr("__RETURN_POINTER__") with CleanExpr
case class SetStmt(varExp: LValue, value: Expr) extends StrRepr(s"$varExp = $value;") with Stmt
case class SetStmtList(sets: Seq[SetStmt]) extends Stmt {
  override def toString = sets.size match {
    case 0 => ""
    case 1 => sets.head.toString
    case _ =>
      sets.map(_.varExp).mkString("(", ", ", ")") + " = " + sets.map(_.value).mkString("(", ", ", ")") + ";"
  }
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
case class LtExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a < $b)", a, b) with BoolExpr
case class GtExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a > $b)", a, b) with BoolExpr
case class SltExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a <$$ $b)", a, b) with BoolExpr
case class SgtExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a >$$ $b)", a, b) with BoolExpr
case class EqExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a == $b)", a, b) with BoolExpr
case class AndExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a & $b)", a, b)
case class OrExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a | $b)", a, b)
case class XorExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"($a ^ $b)", a, b)
case class BitwiseNotExpr(a: Expr) extends MaybeDirtyExpr(s"~$a", a)
case class NotExpr(a: Expr) extends MaybeDirtyExpr(s"!$a") with BoolExpr
case class ByteExpr(a: Expr, b: Expr) extends MaybeDirtyExpr(s"bytes32($a)[$b]")
case class SHA3Expr(exprs: Expr*) extends StrRepr(s"sha3(${exprs.mkString(", ")}])") with DirtyExpr
case object AddressExpr extends StrRepr("this") with CleanExpr
case class BalanceExpr(a: Expr) extends StrRepr(s"$a.balance") with DirtyExpr
case object OriginExpr extends StrRepr("tx.origin") with CleanExpr
case object CallerExpr extends StrRepr("msg.sender") with CleanExpr
case object CallvalueExpr extends StrRepr("msg.value") with CleanExpr
case object CalldatasizeExpr extends StrRepr("msg.data.length") with CleanExpr
case class CalldataExpr(start: Expr, end: Expr) extends StrRepr(s"msg.DATA[$start..$end]") with CleanExpr
case object GasPriceExpr extends StrRepr("tx.gasprice") with CleanExpr
case class CodesizeExpr(a: Expr) extends StrRepr(s"$a.CODE.length") with DirtyExpr
case class CodeExpr(addr: Expr, start: Expr, end: Expr) extends StrRepr(s"$addr.CODE[$start..$end]") with DirtyExpr
case class BlockhashExpr(a: Expr) extends StrRepr(s"block.blockhash($a)") with CleanExpr
case object CoinbaseExpr extends StrRepr("block.coinbase") with CleanExpr
case object TimestampExpr extends StrRepr("block.timestamp") with CleanExpr
case object NumberExpr extends StrRepr("block.number") with CleanExpr
case object DifficultyExpr extends StrRepr("block.difficulty") with CleanExpr
case object GaslimitExpr extends StrRepr("block.gaslimit") with CleanExpr
case class MemoryExpr(start: Expr, end: Expr) extends StrRepr(s"MEMORY[$start..$end]") with DirtyExpr with LValue
case class StorageExpr(a: Expr) extends StrRepr(s"STORAGE[$a]") with DirtyExpr with LValue
case object PcExpr extends StrRepr("__PROGRAM_COUNTER__") with CleanExpr
case object MsizeExpr extends StrRepr("MEMORY.length") with DirtyExpr
case object GasExpr extends StrRepr("msg.gas") with DirtyExpr
case class JumpDestStmt(i: Int) extends StrRepr(f"TAG_$i%x:") with Stmt
case class ConstExpr(a: BigInt) extends StrRepr(f"0x$a%x") with CleanExpr
case class LogStmt(data: Expr, topics: Expr*) extends StrRepr(s"LOG(${topics.mkString(", ")}, $data)") with Stmt
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
      case ConstExpr(n) => f"func_$n%04x"
      case e => s"($e)"
    }, inputs, outputs)
  }
  override def toString = s"(${outputs.mkString(", ")}) = $name(${inputs.mkString(", ")})"
}

case class CommentStmt(content: String) extends StrRepr(s"// $content") with Stmt

case class FunctionAST(name: String, inputs: Int, outputs: Int, stmts: List[Stmt]) {
  override def toString = {
    val vars = ((0 until inputs) map ArgExpr).mkString(", ")
    val returns = Seq.fill(outputs)("int").mkString(", ")
    val code = stmts.map("  " + _).mkString("\n  ")
    s"function $name($vars) returns ($returns) {\n  $code\n}\n\n"
  }
}

case object TrueExpr extends StrRepr("true") with CleanExpr with BoolExpr
case object FalseExpr extends StrRepr("false") with CleanExpr with BoolExpr


object AST {
  private val IntSize = BigInt(2).pow(256)
  def funcsToAst(funcs: Set[Func], signatureHints: Set[SignatureHint]) = {
    for (Func(address, code, inputs, outputs, startingStates) <- funcs) yield {
      val startStacks = for ((addr, startStack) <- startingStates) yield {
        val spaceForReturnPointer = if (outputs >= 0) 1 else 0
        val normStack = startStack.ensureDepth(startStack.height + inputs + spaceForReturnPointer)
        addr -> (normStack.vars.zipWithIndex map {
          case (StackVar(`inputs`), _) => ReturnLocationExpr
          case (StackVar(n), _) => ArgExpr(inputs - n - 1)
          case (CfConst(n), _) => ConstExpr(n)
          case (CalculatedExpr, i) => VarExpr(normStack.vars.size - i)
        })
      }
      FunctionAST(f"func_$address%04x", inputs, outputs,
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
            assert(exprStack.nonEmpty)
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

          def jumpStmtList(nextLoc: Expr, inputStack: List[Expr]): List[Stmt] = {
            nextLoc match {
              case ReturnLocationExpr => List(FunctionReturnStmt(inputStack: _*))
              case ConstExpr(n) if startStacks contains n.toInt =>
                // Local jump
                val blockEntryStack = startStacks(n.toInt)
                List(
                  SetStmtList(for {
                    (expected, actual) <- blockEntryStack zip inputStack
                    if !(expected == actual || (expected == ReturnLocationExpr))
                  } yield SetStmt(expected.asInstanceOf[LValue], actual)),
                  GotoStmt(ConstExpr(n))
                )
              case ConstExpr(n) if funcs.exists(_.address == n.toInt) =>
                val func = funcs.find(_.address == n.toInt).get
                val (inputExprs, restStack) = inputStack.splitAt(func.inputs)
                val outputExprs = List.fill(math.max(func.outputs, 0))(newVar())
                // FIXME Do output expressions need to go on the stack???
                val continueStmt =
                  if (func.outputs >= 0) {
                    val returnExpr :: nextStack = restStack
                    jumpStmtList(returnExpr, outputExprs ::: nextStack)
                  }
                  else List(CommentStmt("*probably* unreachable"), FunctionReturnStmt())

                List(new FunctionCallStmt(ConstExpr(func.address), inputExprs, outputExprs)) ++ continueStmt
              case ConstExpr(n) if n.toInt == 2 =>
                // This is an idiom from older Solidity compilers
                List(ThrowStmt)
              case x =>
                signatureHints collectFirst {
                  case SignatureHint(`address`, funcInputs, funcOutputs, returnAddr) =>
                    val (inputExprs, restStack) = inputStack.splitAt(funcInputs)
                    val outputExprs = List.fill(funcOutputs)(newVar())
                    val returnExpr :: nextStack = restStack
                    assert(returnExpr == ConstExpr(returnAddr))
                    exprStack = outputExprs ++ inputStack
                    // FIXME Do output expressions need to go on the stack???
                    List(new FunctionCallStmt(x, inputExprs, outputExprs)) ++ jumpStmtList(ConstExpr(returnAddr), nextStack)
                } getOrElse {
                  List(
                    CommentStmt("Wild GOTO"),
                    GotoStmt(x)
                  )
                }
            }
          }

          def constFoldBinOp(constFunc: (BigInt, BigInt) => BigInt,
                             fallbackFunc: (Expr, Expr) => Expr,
                             unit: BigInt = null) = {
            push(
              (pop(), pop()) match {
                case (a, ConstExpr(`unit`)) => a
                case (ConstExpr(a), ConstExpr(b)) => ConstExpr(constFunc(a, b))
                case (a, b) => fallbackFunc(a, b)
              }
            )
          }

          def addExprs(a: Expr, b: Expr) = (a, b) match {
            case (ConstExpr(a), ConstExpr(b)) => ConstExpr((a + b) % IntSize)
            case (a, b) => AddExpr(a, b)
          }

          for ((i, inst) <- instructions) {
            inst match {
              case STOP => stmts += StopStmt
              case ADD => constFoldBinOp((a, b) => (a + b) % IntSize, AddExpr, 0)
              case MUL => constFoldBinOp((a, b) => (a + b) % IntSize, MulExpr, 1)
              case SUB => constFoldBinOp((a, b) => (a - b) % IntSize, SubExpr, 0)
              case DIV => constFoldBinOp((a, b) => (a / b), DivExpr, 1)
              case SDIV => push(SdivExpr(pop(), pop())) // Leave this uncommon case for now
              case MOD => constFoldBinOp((a, b) => a % b, ModExpr)
              case SMOD => push(SmodExpr(pop(), pop())) // Leave this uncommon case for now
              case ADDMOD => push(AddmodExpr(pop(), pop(), pop()))
              case MULMOD => push(MulmodExpr(pop(), pop(), pop()))
              case EXP => constFoldBinOp((a, b) => (a pow b.intValue) % IntSize, ExpExpr, 1)
              case SIGNEXTEND => push(SignExtendExpr(pop()))
              case LT => push(LtExpr(pop(), pop()))
              case GT => push(GtExpr(pop(), pop()))
              case SLT => push(SltExpr(pop(), pop()))
              case SGT => push(SgtExpr(pop(), pop()))
              case EQ => push(EqExpr(pop(), pop()))
              case ISZERO => push(pop() match {
                case a: BoolExpr => NotExpr(a)
                case a => EqExpr(a, ConstExpr(0))
              })
              case AND => constFoldBinOp((a, b) => a & b, AndExpr, IntSize - 1)
              case OR => constFoldBinOp((a, b) => a | b, OrExpr, 0)
              case XOR => constFoldBinOp((a, b) => a ^ b, XorExpr, 0)
              case NOT =>
                push(pop() match {
                  case ConstExpr(a) => ConstExpr(a ^ (IntSize - 1))
                  case a => BitwiseNotExpr(a)
                })
              case BYTE => push(ByteExpr(pop(), pop()))
              case SHA3 =>
                val memStart = pop()
                val length = pop()
                val memEnd = addExprs(memStart, length)

                push(SHA3Expr(MemoryExpr(memStart, memEnd)))
              case ADDRESS => push(AddressExpr)
              case BALANCE => push(BalanceExpr(pop()))
              case ORIGIN => push(OriginExpr)
              case CALLER => push(CallerExpr)
              case CALLVALUE => push(CallvalueExpr)
              case CALLDATALOAD =>
                val start = pop()
                val end = addExprs(start, ConstExpr(32))
                push(CalldataExpr(start, end))
              case CALLDATASIZE => push(CalldatasizeExpr)
              case CALLDATACOPY =>
                flushStack()
                val memStart = pop()
                val codeStart = pop()
                val length = pop()
                val memEnd = addExprs(memStart, length)
                val codeEnd = addExprs(codeStart, length)
                stmts += SetStmt(MemoryExpr(memStart, memEnd), CalldataExpr(codeStart, codeEnd))
              case CODESIZE => push(CodesizeExpr(AddressExpr))
              case CODECOPY =>
                flushStack()
                val memStart = pop()
                val codeStart = pop()
                val length = pop()
                val memEnd = addExprs(memStart, length)
                val codeEnd = addExprs(codeStart, length)
                stmts += SetStmt(MemoryExpr(memStart, memEnd), CodeExpr(AddressExpr, codeStart, codeEnd))
              case GASPRICE => push(GasPriceExpr)
              case EXTCODESIZE => push(CodesizeExpr(pop()))
              case EXTCODECOPY =>
                flushStack()
                val address = pop()
                val memStart = pop()
                val codeStart = pop()
                val length = pop()
                val memEnd = addExprs(memStart, length)
                val codeEnd = addExprs(codeStart, length)
                stmts += SetStmt(MemoryExpr(memStart, memEnd), CodeExpr(address, codeStart, codeEnd))
              case BLOCKHASH => push(BlockhashExpr(pop()))
              case COINBASE => push(CoinbaseExpr)
              case TIMESTAMP => push(TimestampExpr)
              case NUMBER => push(NumberExpr)
              case DIFFICULTY => push(DifficultyExpr)
              case GASLIMIT => push(GaslimitExpr)
              case POP => pop()
              case MLOAD =>
                val start = pop()
                val end = addExprs(start, ConstExpr(32))
                push(MemoryExpr(start, end))
              case MSTORE =>
                flushStack()
                val start = pop()
                val end = addExprs(start, ConstExpr(32))
                val value = pop()
                stmts += SetStmt(MemoryExpr(start, end), value)
              case MSTORE8 =>
                flushStack()
                val start = pop()
                val end = addExprs(start, ConstExpr(8))
                val value = pop()
                stmts += SetStmt(MemoryExpr(start, end), value)
              case SLOAD => push(StorageExpr(pop()))
              case SSTORE =>
                flushStack()
                val a = pop()
                val b = pop()
                stmts += SetStmt(StorageExpr(a), b)
              case JUMP =>
                flushStack()
                stmts ++= jumpStmtList(pop(), exprStack)
              case JUMPI =>
                flushStack()
                val loc = pop()
                val cond = pop()
                stmts += IfStmt(cond, StmtList(jumpStmtList(loc, exprStack)))
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
                flushStack()
                val a = pop()
                val b = pop()
                val topics = Seq.fill(n)(pop())
                stmts += LogStmt(MemoryExpr(a, addExprs(a, b)), topics: _*)
              case CREATE =>
                flushStack()
                val a = pop()
                val b = pop()
                val c = pop()
                val returnVar = newVar()
                stmts += CreateStmt(returnVar, a, b, c)
              case CALL =>
                flushStack()
                val gas = pop()
                val addr = pop()
                val value = pop()
                val inOffset = pop()
                val inLength = pop()
                val outOffset = pop()
                val outLength = pop()
                stmts += CallStmt(gas, addr, value, inOffset, inLength, outOffset, outLength)
                push(OkVarExpr)
              case CALLCODE =>
                flushStack()
                val gas = pop()
                val addr = pop()
                val value = pop()
                val inOffset = pop()
                val inLength = pop()
                val outOffset = pop()
                val outLength = pop()
                stmts += CallcodeStmt(gas, addr, value, inOffset, inLength, outOffset, outLength)
                push(OkVarExpr)
              case RETURN =>
                stmts += ReturnStmt(pop(), pop())
              case DELEGATECALL =>
                flushStack()
                val gas = pop()
                val addr = pop()
                val inOffset = pop()
                val inLength = pop()
                val outOffset = pop()
                val outLength = pop()
                stmts += DelegatecallStmt(gas, addr, inOffset, inLength, outOffset, outLength)
                push(OkVarExpr)
              case INVALID|UNKNOWN => stmts += ThrowStmt
              case REVERT => stmts += RevertStmt(pop(), pop())
              case SELFDESTRUCT => stmts += SelfdestructStmt(pop())
            }
          }

          // Optionally prepare for fallthrough
          (stateChange.exitPoint match {
            case Fallthrough(n) => Some(n)
            case ConditionalExit(_, Fallthrough(n)) => Some(n)
            case _ => None
          }) foreach {n =>
            if (startStacks contains n) {
              val startStack = startStacks(n)
              assert(startStack.size == exprStack.size)
              // FIXME: Deal with damn-weird corner case when fallthrough has inconsistent stack height
              stmts += SetStmtList(for {
                (expected, actual) <- startStack zip exprStack
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
