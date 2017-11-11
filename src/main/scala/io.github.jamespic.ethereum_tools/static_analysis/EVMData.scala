package io.github.jamespic.ethereum_tools.static_analysis

import scala.collection.SortedMap
import org.ethereum.crypto.HashUtil.{sha3, sha256, ripemd160}
import org.ethereum.util.BIUtil.toBI

import javax.xml.bind.DatatypeConverter.parseHexBinary

class Str(s: => String) { override def toString = s }
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
    case (Constant(_), DefenderControlledData) => DefenderControlledData
    case (DefenderControlledData, Constant(_)) => DefenderControlledData
    case (DefenderControlledData, DefenderControlledData) => DefenderControlledData
    case (Constant(a), Constant(b)) => Constant(a + b)
    case (Constant(a), b) if a == 0 => b
    case (a, Constant(b)) if b == 0 => a
    case (AddExpr(a, Constant(b)), Constant(c)) => AddExpr(a, Constant(b + c)) // Associate constants and add
    case (Constant(a), b) => b + Constant(a) // commute constants to the right
    case (a, Constant(b)) => AddExpr(a, Constant(b)) // commute constants to the right
    case (a, b) if a.hashCode < b.hashCode => AddExpr(a, b)
    case (a, b) => AddExpr(b, a)
  }
  def *(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(a * b)
    case (Constant(a), b) if a == 1 => b
    case (Constant(a), b) if a == 0 => Constant(0)
    case (a, Constant(b)) => Constant(b) * a // Commute constants to the left
    case (Constant(a), MulExpr(Constant(b), c)) => MulExpr(Constant(a * b), c) // Associate constants and multiply
    case (Constant(a), AddExpr(b, Constant(c))) => AddExpr(Constant(a) * b, Constant(a * c)) // Distribute into add expressions with consts
    case (Constant(a), b) => MulExpr(Constant(a), b)
    case (a, b) if a.hashCode < b.hashCode => MulExpr(a, b)
    case (a, b) => MulExpr(b, a)
  }
  def -(that: EVMData): EVMData = (this, that) match {
    case (Constant(a), Constant(b)) => Constant(a - b)
    case (a, b) if a == b => Constant(0)
    case (a, Constant(b)) if b == 0 => a
    case (a, Constant(b)) => a + Constant(-b)
      // Should we turn Constant(a) - b into (-b) + Constant(a) ?? Probably not common enough to be worth it
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
      case CallData(start, length, callId) =>
        val newLength = length match {
          case Constant(l) => l.toInt min (endBytes - startBytes)
          case _ => endBytes - startBytes
        }
        CallData(start + startBytes, newLength, callId)
      case Constant(n) => Constant((u(n) >> ((32 - endBytes) * 8)) & mask)
      case a => AndExpr(DivExpr(a, Constant(1 << ((32 - endBytes) * 8))), mask)
    }
  }
  def isConstant = this match {
    case Constant(_) => true
    case _ => false
  }
}
case class Constant(n: BigInt) extends EVMData with HashMemo {
  override def toString = "0x" + n.toString(16)
}

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

trait AttackerControlledMemory
  extends AttackerControlled with MemoryLike {
  val start: EVMData
  val length: EVMData
  val callId: Int

  override def toString = callId match {
    case 0 => s"${getClass.getSimpleName}($start, $length)"
    case a => s"${getClass.getSimpleName}_${a}($start, $length)"
  }

  def getRange(rStart: EVMData, rLength: EVMData) = {
    val sliced = slice(rStart, rLength)
    val rangeLength = rLength match {
      case Constant(l) => l.toInt
      case _ => 32 // Hope no-one looks too closely
    }
    SortedMap(MemRange(0, rangeLength) -> sliced)
  }

  def slice(rStart: EVMData, rLength: EVMData) = {
    val newLength = (rStart, rLength, length) match {
      case (Constant(rS), Constant(rL), Constant(l)) if l < rS + rL => Constant(l)
      case _ => rLength
    }
    CallData(start + rStart, newLength, callId)
  }

  def get(start: EVMData, len: EVMData) = slice(start, len)

  def put(key: EVMData, value: EVMData) = promote.put(key, value)

  def putRange(start: EVMData, length: EVMData, data: Iterable[(MemRange, EVMData)]) = {
    promote.putRange(start, length, data)
  }

  def getBinary(start: EVMData, length: EVMData): Array[Byte] = new Array[Byte](guessLength(length))

  val binary: Array[Byte] = new Array[Byte](guessLength(length))

  protected def promote = {
    Memory().putRange(0, length, Iterable(MemRange(0, guessLength(length)) -> this))
  }

  def guessLength(l: EVMData) = l match {
    case Constant(l) => l.toInt
    case _ => 256
  }
}

case class CallData(start: EVMData = 0,
                    length: EVMData = CallDataLength(0),
                    callId: Int = 0) extends AttackerControlledMemory with HashMemo

case class CallDataLength(callId: Int) extends AttackerControlled with HashMemo
case object AttackerControlledAddress extends AttackerControlled with HashMemo
case class AttackerReturnData(start: EVMData, length: EVMData, callId: Int) extends AttackerControlledMemory with HashMemo
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
case object Gas extends AttackerControlled with HashMemo // This is shit, but probably doesn't matter
sealed trait DefenderControlled extends EVMData
case object DefenderControlledData extends DefenderControlled with HashMemo
case object DefenderControlledAddress extends DefenderControlled with HashMemo
class Timestamp(ts: Long) extends Constant(ts) with DefenderControlled {
  override def toString = "TIMESTAMP"
}
class Blocknumber(n: Long) extends Constant(n) with DefenderControlled {
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
class BinaryConstant(val binData: Array[Byte]) extends Constant(toBI(binData)) with MemoryLike {
//  override def toString = s"BinaryConstant(<binary>)"

  override def getRange(start: EVMData, length: EVMData): SortedMap[MemRange, EVMData] = {
    val newBinData = getBinary(start, length)
    SortedMap(MemRange(0, newBinData.length) -> BinaryConstant(newBinData))
  }

  override def slice(start: EVMData, length: EVMData): MemoryLike = BinaryConstant(getBinary(start, length))

  override def get(start: EVMData, len: EVMData): EVMData = BinaryConstant(getBinary(start, len))

  override def put(key: EVMData, value: EVMData): MemoryLike = promote.put(key, value)

  override def putRange(start: EVMData, length: EVMData, data: Iterable[(MemRange, EVMData)]) = {
    promote.putRange(start, length, data)
  }

  override def getBinary(start: EVMData, length: EVMData): Array[Byte] = (start, length) match {
    case (Constant(s), Constant(l)) => binData.slice(s.toInt, l.toInt + s.toInt)
    case (_, Constant(l)) => new Array[Byte](l.toInt)
    case _ => new Array[Byte](0)
  }

  private def promote = {
    Memory().putRange(0, binData.length, Iterable(MemRange(0, binData.length) -> this))
  }

  override val binary: Array[Byte] = binData
}

sealed trait Hash extends EVMData {
  val data: Seq[EVMData]
}

case class VarKeccak256(data: Seq[EVMData]) extends Str(s"Keccak256(${data.mkString(", ")})") with Hash with HashMemo
class ConstantKeccak256(val data: Seq[EVMData], binData: Array[Byte]) extends BinaryConstant(sha3(binData)) with Hash {
  override def toString = s"Keccak256(${data.mkString(", ")})"
}

case class VarSHA256(data: Seq[EVMData]) extends Str(s"SHA256(${data.mkString(", ")})") with Hash with HashMemo
class ConstantSHA256(val data: Seq[EVMData], binData: Array[Byte]) extends BinaryConstant(sha256(binData)) with Hash {
  override def toString = s"SHA256(${data.mkString(", ")})"
}

case class VarRipemd(data: Seq[EVMData]) extends Str(s"Ripemd(${data.mkString(", ")})") with Hash with HashMemo
class ConstantRipemd(val data: Seq[EVMData], binData: Array[Byte]) extends BinaryConstant(ripemd160(binData)) with Hash {
  override def toString = s"Ripemd160(${data.mkString(", ")})"
}
