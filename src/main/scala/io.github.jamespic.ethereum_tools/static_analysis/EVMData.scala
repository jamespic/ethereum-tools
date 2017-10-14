package io.github.jamespic.ethereum_tools.static_analysis

import scala.collection.SortedMap
import org.ethereum.crypto.HashUtil.sha3
import org.ethereum.util.BIUtil.toBI

import javax.xml.bind.DatatypeConverter.parseHexBinary

class Str(s: String) { override def toString = s }
object EVMData {
  private val modulus = BigInt(2).pow(256)
  private val signedUpperLimit = modulus / 2 - 1

  private def u(x: BigInt) = if (x < 0) x + modulus else x
  private def s(x: BigInt) = if (x > signedUpperLimit) x - modulus else x
  private def bool(x: Boolean) = if (x) Constant(1) else Constant(0)

  private def overflowingConstant(n: BigInt) = if (-128 < n && n < 1024) Constant(n) else DefenderControlledData
  implicit def intToConstant(i: Int): EVMData = Constant(BigInt(i))
  implicit def bigIntToConstant(i: BigInt): EVMData = Constant(i)
}
sealed trait EVMData {
  import EVMData._
  def +(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => overflowingConstant(a + b)
    case (Constant(a), b) if a == 0 => b
    case (a, Constant(b)) if b == 0 => a
    case (a, b) if a.hashCode < b.hashCode => AddExpr(a, b)
    case (a, b) => AddExpr(b, a)
  }
  def *(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => overflowingConstant(a * b)
    case (Constant(a), b) if a == 1 => b
    case (a, Constant(b)) if b == 1 => a
    case (a, b) if a.hashCode < b.hashCode => MulExpr(a, b)
    case (a, b) => MulExpr(b, a)
  }
  def -(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => overflowingConstant(a - b)
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
    case (Constant(a), Constant(b), Constant(c)) => overflowingConstant((a + b) % c)
    case (a, b, c) if a.hashCode < b.hashCode => ModExpr(AddExpr(a, b), c)
    case (a, b, c) => ModExpr(AddExpr(b, a), c)
  }
  def mulmod(that: EVMData, m: EVMData): EVMData = (this, that, m) match {
    case (Constant(a), Constant(b), Constant(c)) => overflowingConstant((a * b) % c)
    case (a, b, c) if a.hashCode < b.hashCode => ModExpr(MulExpr(a, b), c)
    case (a, b, c) => ModExpr(MulExpr(b, a), c)
  }
  def **(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => overflowingConstant(a.modPow(b, modulus))
    case (a, b) => ExpExpr(a, b)
  }
  def <(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => bool(u(a) < u(b))
    case (a, b) => LessThan(a, b)
  }
  def slt(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => bool(s(a) < s(b))
    case (a, b) => LessThan(a, b)
  }
  def >(that: EVMData): EVMData = that < this
  def sgt(that: EVMData): EVMData = that slt this
  def ===(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => bool(u(a) == u(b))
    case (a, b) if a.hashCode < b.hashCode => Equals(a, b)
    case (a, b) => Equals(b, a)
  }
  def unary_! : EVMData = this match {
    case Constant(a) => bool(a == 0)
    case (a: Predicate) => Not(a)
    case a => a === Constant(0)
  }
  def &(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(u(a) & u(b))
    case (x, Constant(d)) if (d & (d + 1)) == 0 && (d + 1).lowestSetBit % 8 == 0 =>
      val byteCount = (d + 1).lowestSetBit / 8
      x.clipHighBytes(byteCount)
    case (a, b) if a.hashCode < b.hashCode => AndExpr(a, b)
    case (a, b) => AndExpr(b, a)
  }
  def |(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(u(a) | u(b))
    case (a, b) if a.hashCode < b.hashCode => OrExpr(a, b)
    case (a, b) => OrExpr(b, a)
  }
  def ^(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(u(a) ^ u(b))
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
      case CallData(start, length) => CallData(start + length - byteCount, length - byteCount)
      case Constant(n) => Constant(u(n) & mask)
      case a => AndExpr(a, Constant(mask))
    }
  }
  def clipLowBytes(byteCount: Int): EVMData = this match {
    case b: BinaryConstant =>
      new BinaryConstant(b.binData.slice(0, b.binData.length - byteCount))
    case CallData(start, length) => CallData(start, length - byteCount)
    case Constant(n) => Constant(u(n) >> (byteCount * 8))
    case a => DivExpr(a, Constant(1 << (byteCount * 8)))
  }
  def subBytes(startBytes: Int, endBytes: Int): EVMData = {
    val mask = BigInt(1) << ((endBytes - startBytes) * 8) - 1
    this match {
      case b: BinaryConstant =>
        new BinaryConstant(b.binData.slice(startBytes, endBytes - startBytes))
      case CallData(start, length) => CallData(start + startBytes, length min (endBytes - startBytes))
      case Constant(n) => Constant((u(n) >> ((32 - endBytes) * 8)) & mask)
      case a => AndExpr(DivExpr(a, Constant(1 << ((32 - endBytes) * 8))), mask)
    }
  }
}
case class Constant(n: BigInt) extends Str("0x" + n.toString(16)) with EVMData

sealed trait AttackerControlled extends EVMData
case object AttackerControlled extends AttackerControlled {
  def unapply(data: EVMData): Boolean = data match {
    case x: AttackerControlled => true
    case BinExpr(AttackerControlled(), b) => true
    case BinExpr(a, AttackerControlled()) => true
    case _ => false
  }
}
case class CallData(start: Int, offset: Int) extends AttackerControlled
case object CallDataLength extends AttackerControlled
case object AttackerControlledAddress extends AttackerControlled
case class AttackerReturnData(start: Int, offset: Int) extends AttackerControlled
object DefenderControlled {
  def unapply(data: EVMData): Boolean = data match {
    case x: DefenderControlled => true
    case BinExpr(DefenderControlled(), b) => true
    case BinExpr(a, DefenderControlled()) => true
    case _ => false
  }
}
sealed trait DefenderControlled extends EVMData
case object DefenderControlledData extends DefenderControlled
case object DefenderControlledAddress extends DefenderControlled
object Timestamp extends Constant(System.currentTimeMillis() / 1000) with DefenderControlled {
  override def toString = "TIMESTAMP"
}
object Blocknumber extends Constant(System.getProperty("blocknumber", "4370000").toLong) with DefenderControlled
case class SpentMoney(n: Int) extends EVMData

// Treat signed and unsigned the same, since these constraints will be solved by hand
object BinExpr {
  def unapply(x: BinExpr) = Some((x.a, x.b))
}
trait BinExpr extends EVMData {
  val a: EVMData
  val b: EVMData
}
case class AddExpr(a: EVMData, b: EVMData) extends Str(s"($a + $b)") with BinExpr
case class MulExpr(a: EVMData, b: EVMData) extends Str(s"($a * $b)") with BinExpr
case class SubExpr(a: EVMData, b: EVMData) extends Str(s"($a - $b)") with BinExpr
case class DivExpr(a: EVMData, b: EVMData) extends Str(s"($a / $b)") with BinExpr
case class ModExpr(a: EVMData, b: EVMData) extends Str(s"($a % $b)") with BinExpr
case class ExpExpr(a: EVMData, b: EVMData) extends Str(s"($a ** $b)") with BinExpr
case class AndExpr(a: EVMData, b: EVMData) extends Str(s"($a & $b)") with BinExpr
case class OrExpr(a: EVMData, b: EVMData) extends Str(s"($a | $b)") with BinExpr
case class XorExpr(a: EVMData, b: EVMData) extends Str(s"($a ^ $b)") with BinExpr
case class BitNotExpr(a: EVMData) extends Str(s"~$a") with EVMData


sealed trait Predicate extends EVMData
case class Equals(a: EVMData, b: EVMData) extends Str(s"($a == $b)") with BinExpr with Predicate
case class LessThan(a: EVMData, b: EVMData) extends Str(s"($a < $b)") with BinExpr with Predicate
case class GreaterThan(a: EVMData, b: EVMData) extends Str(s"($a > $b)") with BinExpr with Predicate
case class Not(a: Predicate) extends Str(s"!$a") with Predicate

sealed trait Hash extends EVMData {
  val data: Seq[EVMData]
}

object BinaryConstant {
  def apply(binData: Array[Byte]) = new BinaryConstant(binData)
  def apply(binData: String) = new BinaryConstant(parseHexBinary(binData))
}
class BinaryConstant(val binData: Array[Byte]) extends Constant(toBI(binData))
case class Keccak256(data: EVMData*) extends Hash
class ConstantKeccak256(val data: Seq[EVMData], binData: Array[Byte]) extends BinaryConstant(sha3(binData)) with Hash {
  override def toString = s"Keccak256(${data.mkString(", ")})"
}

