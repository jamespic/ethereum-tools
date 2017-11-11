package io.github.jamespic.ethereum_tools.static_analysis

import scala.collection.SortedMap
import org.ethereum.vm.PrecompiledContracts.{BN128Addition, BN128Multiplication, BN128Pairing, ModExp, PrecompiledContract => EJPC}

object PrecompiledContracts {
  case class PrecompiledResult(success: Boolean, data: MemoryLike)
  type Precompiled = (MemoryLike, EVMData) => Seq[PrecompiledResult]
  val Contracts: Map[Int, Precompiled] = Map(
    1 -> ecRecover _,
    2 -> sha256 _,
    3 -> ripemd _,
    4 -> memcopy _,
    5 -> modexp _,
    6 -> bnAdd _,
    7 -> bnMul _,
    8 -> bnPair _
  )
  def ecRecover(memory: MemoryLike, length: EVMData): Seq[PrecompiledResult] = {
    val (m, v, r, s) = (memory.get(0), memory.get(32), memory.get(64), memory.get(96))
    val successResults = (m, v, r, s) match {
      case (AttackerControlled(), AttackerControlled(), AttackerControlled(), AttackerControlled()) =>
        Seq(
          PrecompiledResult(
            true,
            Memory(MemRange(0, 32) -> AttackerControlledAddress)
          )
        )
      case (DefenderControlled(), DefenderControlled(), DefenderControlled(), DefenderControlled()) =>
        Seq(
          PrecompiledResult(
            true,
            Memory(MemRange(0, 32) -> DefenderControlledAddress)
          )
        )
      case _ => Nil
    }
    successResults
  }
  def sha256(memory: MemoryLike, length: EVMData): Seq[PrecompiledResult] = {
    val values = memory.getRange(0, length).values.toSeq
    val hash = if (values.forall(_.isConstant)) {
      new ConstantSHA256(
        values,
        memory.getBinary(0, length)
      )
    } else VarSHA256(values)

    Seq(PrecompiledResult(true, Memory(MemRange(0, 32) -> hash)))
  }
  def ripemd(memory: MemoryLike, length: EVMData): Seq[PrecompiledResult] = {
    val values = memory.getRange(0, length).values.toSeq
    val hash = if (values.forall(_.isConstant)) {
      new ConstantRipemd(
        values,
        memory.getBinary(0, length)
      )
    } else VarRipemd(values)

    Seq(PrecompiledResult(true, Memory(MemRange(0, 32) -> hash)))
  }
  def memcopy(memory: MemoryLike, length: EVMData) = Seq(PrecompiledResult(true, memory.slice(0, length)))
  def modexp(memory: MemoryLike, length: EVMData): Seq[PrecompiledResult] = {
    val baseLen = memory.get(0)
    val expLen = memory.get(32)
    val modLen = memory.get(64)
    (baseLen, expLen, modLen) match {
      case (Constant(_), Constant(_), Constant(_)) =>
        // Guess everything's constant, and use the precompiles from ethereumj
        wrapEthereumJ(memory, new ModExp)

      case _ => // Otherwise, evaluate it symbolically - and accept that some of the numbers will be a bit wrong
        val base = memory.get(96, baseLen)
        val exp = memory.get(96 + baseLen, expLen)
        val mod = memory.get(96 + baseLen + expLen, modLen)
        val symbolicResult = ModExpr(ExpExpr(base, exp), mod)
        Seq(PrecompiledResult(true, Memory(MemRange(0, 32) -> symbolicResult)))
    }
  }
  def bnAdd(memory: MemoryLike, length: EVMData): Seq[PrecompiledResult] = {
    if (memory.getRange(0, 128).values.forall(_.isConstant)) {
      wrapEthereumJ(memory, new BN128Addition())
    } else {
      val x1 = memory.get(0)
      val y1 = memory.get(32)
      val x2 = memory.get(64)
      val y2 = memory.get(96)
      val result = CurvePoint(x1, y1) + CurvePoint(x2, y2)
      Seq(
        PrecompiledResult(true, Memory(MemRange(0, 64) -> result))
      )
    }

  }
  def bnMul(memory: MemoryLike, length: EVMData): Seq[PrecompiledResult] = {
    if (memory.getRange(0, 96).values.forall(_.isConstant)) {
      wrapEthereumJ(memory, new BN128Multiplication())
    } else {
      val x = memory.get(0)
      val y = memory.get(32)
      val s = memory.get(64)
      val result = MulExpr(s, CurvePoint(x, y))
      Seq(
        PrecompiledResult(true, Memory(MemRange(0, 64) -> result))
      )
    }

  }
  def bnPair(memory: MemoryLike, length: EVMData): Seq[PrecompiledResult] = {
    // I have no fucking clue what a pairing is, so until I do, I can't do anything smarter than letting EthereumJ handle this
    wrapEthereumJ(memory, new BN128Pairing())
  }
  private def wrapEthereumJ(memory: MemoryLike, contract: EJPC) = {
    val result = contract.execute(memory.binary)
    val success = result.getLeft.booleanValue
    val output = result.getRight
    Seq(PrecompiledResult(success, Memory(MemRange(0,output.length) -> BinaryConstant(output))))
  }
}
