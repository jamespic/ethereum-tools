package io.github.jamespic.ethereum_tools.static_analysis

import scala.collection.SortedMap
import org.ethereum.crypto.HashUtil.{sha3, sha256, ripemd160}
import org.ethereum.util.BIUtil.toBI

import javax.xml.bind.DatatypeConverter.parseHexBinary

class Str(s: String) { override def toString = s }
object EVMData {
  private val modulus = BigInt(2).pow(256)
  private val signedUpperLimit = modulus / 2 - 1

  private def u(x: BigInt) = if (x < 0) x + modulus else x
  private def s(x: BigInt) = if (x > signedUpperLimit) x - modulus else x
  private def bool(x: Boolean): Predicate = if (x) True else False

  implicit def intToConstant(i: Int): EVMData = Constant(BigInt(i))
  implicit def bigIntToConstant(i: BigInt): EVMData = Constant(i)
}
sealed trait EVMData {
  import EVMData._
  def +(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) if a == 1 && b >= 100 => DefenderControlledData
    case (Constant(a), Constant(b)) if b == 1 && a >= 100 => DefenderControlledData
    case (Constant(a), Constant(b)) => Constant(a + b)
    case (Constant(a), b) if a == 0 => b
    case (a, Constant(b)) if b == 0 => a
    case (a, b) if a.hashCode < b.hashCode => AddExpr(a, b)
    case (a, b) => AddExpr(b, a)
  }
  def +!(that: EVMData): EVMData = (this, that) match {
    // Unsafe form that can grow indefinitely - only for internal use
    case (Constant(a), Constant(b)) => Constant(a + b)
    case (Constant(a), b) if a == 0 => b
    case (a, Constant(b)) if b == 0 => a
    case (a, b) if a.hashCode < b.hashCode => AddExpr(a, b)
    case (a, b) => AddExpr(b, a)
  }
  def *(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(a * b)
    case (Constant(a), b) if a == 1 => b
    case (a, Constant(b)) if b == 1 => a
    case (a, b) if a.hashCode < b.hashCode => MulExpr(a, b)
    case (a, b) => MulExpr(b, a)
  }
  def -(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(a - b)
    case (a, Constant(b)) if b == 0 => a
    case (a, b) => SubExpr(a, b)
  }
  def /(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(u(a) / u(b))
    case (a, Constant(b)) if b == 1 => a
    case (x, Constant(d)) if (d & (d - 1)) == 0 && (d.lowestSetBit % 8 == 0) =>
      val offset = d.lowestSetBit / 8
      x.clipLowBytes(offset)
    case (a, b) => DivExpr(a, b)
  }
  def sdiv(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(s(a) / s(b))
    case (a, Constant(b)) if b == 1 => a
    case (a, b) => DivExpr(a, b)
  }
  def %(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(u(a) % u(b))
    case (a, b) => ModExpr(a, b)
  }
  def smod(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(s(a) % s(b))
    case (a, b) => ModExpr(a, b)
  }
  def addmod(that: EVMData, m: EVMData): EVMData = (this, that, m) match {
    case (Constant(a), Constant(b), Constant(c)) => Constant((a + b) % c)
    case (a, b, c) if a.hashCode < b.hashCode => ModExpr(AddExpr(a, b), c)
    case (a, b, c) => ModExpr(AddExpr(b, a), c)
  }
  def mulmod(that: EVMData, m: EVMData): EVMData = (this, that, m) match {
    case (Constant(a), Constant(b), Constant(c)) => Constant((a * b) % c)
    case (a, b, c) if a.hashCode < b.hashCode => ModExpr(MulExpr(a, b), c)
    case (a, b, c) => ModExpr(MulExpr(b, a), c)
  }
  def **(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(a.modPow(b, modulus))
    case (a, b) => ExpExpr(a, b)
  }
  def <(that: EVMData): Predicate = (this, that) match {
    case (Constant(a), Constant(b)) => bool(u(a) < u(b))
    case (a, b) => LessThan(a, b)
  }
  def <=(that: EVMData): Predicate = (this, that) match {
    case (Constant(a), Constant(b)) => bool(u(a) <= u(b))
    case (a, b) => LessOrEqual(a, b)
  }
  def slt(that: EVMData): Predicate = (this, that) match {
    case (Constant(a), Constant(b)) => bool(s(a) < s(b))
    case (a, b) => LessThan(a, b)
  }
  def >(that: EVMData): Predicate = that < this
  def >=(that: EVMData): Predicate = that <= this
  def sgt(that: EVMData): Predicate = that slt this
  def ===(that: EVMData): Predicate = (this, that) match {
    case (Constant(a), Constant(b)) => bool(u(a) == u(b))
    case (a, b) if a.hashCode < b.hashCode => Equals(a, b)
    case (a, b) => Equals(b, a)
  }
  def unary_! : Predicate = this match {
    case Constant(a) => bool(a == 0)
    case Not(a: Predicate) => a
    case (a: Predicate) => Not(a)
    case a => a === 0
  }
  def &(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(u(a) & u(b))
    case (Constant(d), x) if (d & (d + 1)) == 0 && (d + 1).lowestSetBit % 8 == 0 =>
      val byteCount = (d + 1).lowestSetBit / 8
      x.clipHighBytes(byteCount)
    case (x, Constant(d)) if (d & (d + 1)) == 0 && (d + 1).lowestSetBit % 8 == 0 =>
      val byteCount = (d + 1).lowestSetBit / 8
      x.clipHighBytes(byteCount)
    case (a, b) if a.hashCode < b.hashCode => AndExpr(a, b)
    case (a, b) => AndExpr(b, a)
  }
  def |(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(u(a) | u(b))
    case (Constant(a), b) if a == 0 => b
    case (a, Constant(b)) if b == 0 => a
    case (a, b) if a.hashCode < b.hashCode => OrExpr(a, b)
    case (a, b) => OrExpr(b, a)
  }
  def ^(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(u(a) ^ u(b))
    case (Constant(a), b) if a == 0 => b
    case (a, Constant(b)) if b == 0 => a
    case (a, b) if a.hashCode < b.hashCode => XorExpr(a, b)
    case (a, b) => XorExpr(b, a)
  }
  def unary_~ : EVMData = this match {
    case Constant(a) => Constant(u(a) ^ modulus - 1)
    case a => BitNotExpr(a)
  }

  def clipHighBytes(byteCount: Int): EVMData = {
    val mask = BigInt(1) << (byteCount * 8) - 1
    this match {
      case b: BinaryConstant =>
        new BinaryConstant(b.binData.slice(b.binData.length - byteCount, b.binData.length))
      case CallData(start, length, callId) => CallData(start + length - byteCount, byteCount, callId)
      case Constant(n) => Constant(u(n) & mask)
      case AttackerControlledAddress if byteCount == 20 => AttackerControlledAddress
      case a => AndExpr(a, Constant(mask))
    }
  }
  def clipLowBytes(byteCount: Int): EVMData = this match {
    case b: BinaryConstant =>
      new BinaryConstant(b.binData.slice(0, b.binData.length - byteCount))
    case CallData(start, length, callId) => CallData(start, length - byteCount, callId)
    case Constant(n) => Constant(u(n) >> (byteCount * 8))
    case a => DivExpr(a, Constant(1 << (byteCount * 8)))
  }
  def subBytes(startBytes: Int, endBytes: Int): EVMData = {
    val mask = BigInt(1) << ((endBytes - startBytes) * 8) - 1
    this match {
      case b: BinaryConstant =>
        new BinaryConstant(b.binData.slice(startBytes, endBytes - startBytes))
      case CallData(start, length, callId) => CallData(start + startBytes, length min (endBytes - startBytes), callId)
      case Constant(n) => Constant((u(n) >> ((32 - endBytes) * 8)) & mask)
      case a => AndExpr(DivExpr(a, Constant(1 << ((32 - endBytes) * 8))), mask)
    }
  }
  def isConstant = this match {
    case Constant(_) => true
    case _ => false
  }
}
case class Constant(n: BigInt) extends Str("0x" + n.toString(16)) with EVMData with HashMemo

sealed trait AttackerControlled extends EVMData
case object AttackerControlled extends AttackerControlled with HashMemo {
  def unapply(data: EVMData): Boolean = data match {
    case x: AttackerControlled => true
    case ExpExpr(AttackerControlled(), AttackerControlled()) => true
    case ExpExpr(_, AttackerControlled()) => false
    case ExpExpr(AttackerControlled(), _) => false
    case MulExpr(_, CurvePoint(_, _)) => false
    case BinExpr(AttackerControlled(), b) => true
    case BinExpr(a, AttackerControlled()) => true
    case Not(AttackerControlled()) => true
    case BitNotExpr(AttackerControlled()) => true
    case _ => false
  }
}
case class CallData(start: Int, offset: Int, callId: Int = 0) extends AttackerControlled with HashMemo {
  override def toString = callId match {
    case 0 => s"CallData($start, $offset)"
    case a => s"CallData_${a}($start, $offset)"
  }
}
case class CallDataLength(callId: Int) extends AttackerControlled with HashMemo
case object AttackerControlledAddress extends AttackerControlled with HashMemo
case class AttackerReturnData(start: Int, offset: Int, call: Int) extends AttackerControlled with HashMemo
object DefenderControlled {
  def unapply(data: EVMData): Boolean = data match {
    case x: DefenderControlled => true
    case BinExpr(DefenderControlled(), b) => true
    case BinExpr(a, DefenderControlled()) => true
    case Not(DefenderControlled()) => true
    case BitNotExpr(DefenderControlled()) => true
    case _ => false
  }
}
case object GasPrice extends AttackerControlled with HashMemo
sealed trait DefenderControlled extends EVMData
case object DefenderControlledData extends DefenderControlled with HashMemo
case object DefenderControlledAddress extends DefenderControlled with HashMemo
object Timestamp extends Constant(System.currentTimeMillis() / 1000) with DefenderControlled {
  override def toString = "TIMESTAMP"
}
object Blocknumber extends Constant(System.getProperty("blocknumber", "4370000").toLong) with DefenderControlled {
  override def toString = "BLOCKNUMBER"
}
case class NewContractAddress(creator: EVMData, count: Int) extends EVMData with HashMemo
case class SpentMoney(callId: Int) extends AttackerControlled with HashMemo

// Treat signed and unsigned the same, since these constraints will be solved by hand
object BinExpr {
  def unapply(x: BinExpr) = Some((x.a, x.b))
}
trait BinExpr extends EVMData {
  val a: EVMData
  val b: EVMData
}
case class AddExpr(a: EVMData, b: EVMData) extends Str(s"($a + $b)") with BinExpr with HashMemo
case class MulExpr(a: EVMData, b: EVMData) extends Str(s"($a * $b)") with BinExpr with HashMemo
case class SubExpr(a: EVMData, b: EVMData) extends Str(s"($a - $b)") with BinExpr with HashMemo
case class DivExpr(a: EVMData, b: EVMData) extends Str(s"($a / $b)") with BinExpr with HashMemo
case class ModExpr(a: EVMData, b: EVMData) extends Str(s"($a % $b)") with BinExpr with HashMemo
case class ExpExpr(a: EVMData, b: EVMData) extends Str(s"($a ** $b)") with BinExpr with HashMemo
case class AndExpr(a: EVMData, b: EVMData) extends Str(s"($a & $b)") with BinExpr with HashMemo
case class OrExpr(a: EVMData, b: EVMData) extends Str(s"($a | $b)") with BinExpr with HashMemo
case class XorExpr(a: EVMData, b: EVMData) extends Str(s"($a ^ $b)") with BinExpr with HashMemo
case class CurvePoint(a: EVMData, b: EVMData) extends BinExpr with HashMemo
case class BitNotExpr(a: EVMData) extends Str(s"~$a") with EVMData with HashMemo


sealed trait Predicate extends EVMData
case class Equals(a: EVMData, b: EVMData) extends Str(s"($a == $b)") with BinExpr with Predicate with HashMemo
case class LessThan(a: EVMData, b: EVMData) extends Str(s"($a < $b)") with BinExpr with Predicate with HashMemo
case class GreaterThan(a: EVMData, b: EVMData) extends Str(s"($a > $b)") with BinExpr with Predicate with HashMemo
case class LessOrEqual(a: EVMData, b: EVMData) extends Str(s"($a <= $b)") with BinExpr with Predicate with HashMemo
case class GreaterOrEqual(a: EVMData, b: EVMData) extends Str(s"($a >= $b)") with BinExpr with Predicate with HashMemo
case class Not(a: Predicate) extends Str(s"!$a") with Predicate with HashMemo
object True extends Constant(1) with Predicate {override def toString = "true"}
object False extends Constant(0) with Predicate {override def toString = "false"}

object BinaryConstant {
  def apply(binData: Array[Byte]) = new BinaryConstant(binData)
  def apply(binData: String) = new BinaryConstant(parseHexBinary(binData))
}
class BinaryConstant(val binData: Array[Byte]) extends Constant(toBI(binData))

sealed trait Hash extends EVMData {
  val data: Seq[EVMData]
}

case class Keccak256(data: EVMData*) extends Hash with HashMemo
class ConstantKeccak256(val data: Seq[EVMData], binData: Array[Byte]) extends BinaryConstant(sha3(binData)) with Hash {
  override def toString = s"Keccak256(${data.mkString(", ")})"
}

case class Sha256(data: EVMData*) extends Hash with HashMemo
class ConstantSHA256(val data: Seq[EVMData], binData: Array[Byte]) extends BinaryConstant(sha256(binData)) with Hash {
  override def toString = s"Sha256(${data.mkString(", ")})"
}

case class Ripemd(data: EVMData*) extends Hash with HashMemo
class ConstantRipemd(val data: Seq[EVMData], binData: Array[Byte]) extends BinaryConstant(ripemd160(binData)) with Hash {
  override def toString = s"Ripemd160(${data.mkString(", ")})"
}