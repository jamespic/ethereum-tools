package io.github.jamespic.ethereum_tools

object Bytecode {
  def parse(code: Array[Byte]) = {
    val ops = List.newBuilder[(Int, Bytecode)]
    var i = 0
    while (i < code.length) {
      ops += i -> ((code(i) & 0xff) match {
        case 0x00 => STOP
        case 0x01 => ADD
        case 0x02 => MUL
        case 0x03 => SUB
        case 0x04 => DIV
        case 0x05 => SDIV
        case 0x06 => MOD
        case 0x07 => SMOD
        case 0x08 => ADDMOD
        case 0x09 => MULMOD
        case 0x0a => EXP
        case 0x0b => SIGNEXTEND
        case 0x10 => LT
        case 0x11 => GT
        case 0x12 => SLT
        case 0x13 => SGT
        case 0x14 => EQ
        case 0x15 => ISZERO
        case 0x16 => AND
        case 0x17 => OR
        case 0x18 => XOR
        case 0x19 => NOT
        case 0x1a => BYTE
        case 0x20 => SHA3
        case 0x30 => ADDRESS
        case 0x31 => BALANCE
        case 0x32 => ORIGIN
        case 0x33 => CALLER
        case 0x34 => CALLVALUE
        case 0x35 => CALLDATALOAD
        case 0x36 => CALLDATASIZE
        case 0x37 => CALLDATACOPY
        case 0x38 => CODESIZE
        case 0x39 => CODECOPY
        case 0x3a => GASPRICE
        case 0x3b => EXTCODESIZE
        case 0x3c => EXTCODECOPY
        case 0x40 => BLOCKHASH
        case 0x41 => COINBASE
        case 0x42 => TIMESTAMP
        case 0x43 => NUMBER
        case 0x44 => DIFFICULTY
        case 0x45 => GASLIMIT
        case 0x50 => POP
        case 0x51 => MLOAD
        case 0x52 => MSTORE
        case 0x53 => MSTORE8
        case 0x54 => SLOAD
        case 0x55 => SSTORE
        case 0x56 => JUMP
        case 0x57 => JUMPI
        case 0x58 => PC
        case 0x59 => MSIZE
        case 0x5a => GAS
        case 0x5b => JUMPDEST
        case x if 0x60 <= x && x <= 0x7f =>
          val len = x - 0x5f
          val data = code.slice(i + 1, i + 1 + len)
          i += len
          PUSH(data)
        case x if 0x80 <= x && x <= 0x8f => DUP(x - 0x7f)
        case x if 0x90 <= x && x <= 0x9f => SWAP(x - 0x8f)
        case x if 0xa0 <= x && x <= 0xa4 => LOG(x - 0xa0)
        case 0xf0 => CREATE
        case 0xf1 => CALL
        case 0xf2 => CALLCODE
        case 0xf3 => RETURN
        case 0xf4 => DELEGATECALL
        case 0xff => SUICIDE
        case _ => UNKNOWN
      })
      i += 1
    }
    ops.result()
  }
}

abstract sealed class Bytecode(val inputs: Int, val outputs: Int)
case object STOP extends Bytecode(0, 0)
case object ADD extends Bytecode(2, 1)
case object MUL extends Bytecode(2, 1)
case object SUB extends Bytecode(2, 1)
case object DIV extends Bytecode(2, 1)
case object SDIV extends Bytecode(2, 1)
case object MOD extends Bytecode(2, 1)
case object SMOD extends Bytecode(2, 1)
case object ADDMOD extends Bytecode(3, 1)
case object MULMOD extends Bytecode(3, 1)
case object EXP extends Bytecode(2, 1)
case object SIGNEXTEND extends Bytecode(2, 1)
case object LT extends Bytecode(2, 1)
case object GT extends Bytecode(2, 1)
case object SLT extends Bytecode(2, 1)
case object SGT extends Bytecode(2, 1)
case object EQ extends Bytecode(2, 1)
case object ISZERO extends Bytecode(1, 1)
case object AND extends Bytecode(2, 1)
case object OR extends Bytecode(2, 1)
case object XOR extends Bytecode(2, 1)
case object NOT extends Bytecode(1, 1)
case object BYTE extends Bytecode(2, 1)
case object SHA3 extends Bytecode(2, 1)
case object ADDRESS extends Bytecode(0, 1)
case object BALANCE extends Bytecode(1, 1)
case object ORIGIN extends Bytecode(0, 1)
case object CALLER extends Bytecode(0, 1)
case object CALLVALUE extends Bytecode(0, 1)
case object CALLDATALOAD extends Bytecode(1, 1)
case object CALLDATASIZE extends Bytecode(0, 1)
case object CALLDATACOPY extends Bytecode(3, 0)
case object CODESIZE extends Bytecode(0, 1)
case object CODECOPY extends Bytecode(3, 0)
case object GASPRICE extends Bytecode(0, 1)
case object EXTCODESIZE extends Bytecode(1, 1)
case object EXTCODECOPY extends Bytecode(4, 0)
case object BLOCKHASH extends Bytecode(1, 1)
case object COINBASE extends Bytecode(0, 1)
case object TIMESTAMP extends Bytecode(0, 1)
case object NUMBER extends Bytecode(0, 1)
case object DIFFICULTY extends Bytecode(0, 1)
case object GASLIMIT extends Bytecode(0, 1)
case object POP extends Bytecode(1, 1)
case object MLOAD extends Bytecode(1, 1)
case object MSTORE extends Bytecode(2, 0)
case object MSTORE8 extends Bytecode(2, 0)
case object SLOAD extends Bytecode(1, 1)
case object SSTORE extends Bytecode(2, 0)
case object JUMP extends Bytecode(1, 0)
case object JUMPI extends Bytecode(2, 0)
case object PC extends Bytecode(0, 1)
case object MSIZE extends Bytecode(0, 1)
case object GAS extends Bytecode(0, 1)
case object JUMPDEST extends Bytecode(0, 0)
case class PUSH(length: Int, data: BigInt) extends Bytecode(0, 1) {
  require(length <= 32)
  override def toString = s"PUSH${length} ${data.toString(16)}"
}
object PUSH {
  def apply(data: String): PUSH = PUSH((data.length + 1) / 2, BigInt(data, 16))
  def apply(data: Seq[Byte]): PUSH = {
    require(data.length <= 32)
    PUSH(data.length, (BigInt(0) /: data)((acc, v) => acc * 256 + (v & 0xff)))
  }
}
case class DUP(depth: Int) extends Bytecode(depth, depth + 1) {
  require(0 < depth && depth <= 16)
  override def toString = s"DUP$depth"
}
case class SWAP(depth: Int) extends Bytecode(depth + 1, depth + 1) {
  require(0 < depth && depth <= 16)
  override def toString = s"SWAP$depth"
}
case class LOG(topics: Int) extends Bytecode(topics + 2, 0) {
  require(0 <= topics && topics <= 4)
  override def toString = s"LOG$topics"
}
case object CREATE extends Bytecode(3, 1)
case object CALL extends Bytecode(7, 1)
case object CALLCODE extends Bytecode(7, 1)
case object RETURN extends Bytecode(2, 0)
case object DELEGATECALL extends Bytecode(6, 1)
case object SUICIDE extends Bytecode(1, 0)
case object UNKNOWN extends Bytecode(0, 0)
