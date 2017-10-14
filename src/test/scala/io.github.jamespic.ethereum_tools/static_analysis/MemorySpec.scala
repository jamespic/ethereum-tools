package io.github.jamespic.ethereum_tools.static_analysis

import org.scalatest.{FreeSpec, Matchers}

import scala.collection.SortedMap

import javax.xml.bind.DatatypeConverter.printHexBinary

class MemorySpec extends FreeSpec with Matchers {
  "Memory" - {
    "intersectingRanges" - {
      val mem = Memory(knownRanges = SortedMap(
        MemRange(0, 5) -> Constant(1),
        MemRange(5, 10) -> Constant(2),
        MemRange(10, 15) -> Constant(3),
        MemRange(15, 20) -> Constant(4),
        MemRange(20, 25) -> Constant(5)
      ))

      "should find ranges that cross boundaries" in {
        mem.intersectingRanges(6, 17) shouldEqual SortedMap(
          MemRange(5, 10) -> Constant(2),
          MemRange(10, 15) -> Constant(3),
          MemRange(15, 20) -> Constant(4)
        )
      }
      "should find ranges at boundaries" in {
        mem.intersectingRanges(10, 15) shouldEqual SortedMap(
          MemRange(10, 15) -> Constant(3)
        )
      }
    }
    "getRange" - {
      val mem = Memory(knownRanges = SortedMap(
        MemRange(0, 4) -> BinaryConstant("01234567"),
        MemRange(4, 8) -> BinaryConstant("89abcdef"),
        MemRange(8, 12) -> BinaryConstant("fedcba98")
      ))
      "should return ranges with content clipped" in {
        mem.getRange(2, 8) shouldEqual SortedMap(
          MemRange(0, 2) -> BinaryConstant("4567"),
          MemRange(2, 6) -> BinaryConstant("89abcdef"),
          MemRange(6, 8) -> BinaryConstant("fedc")
        )
      }
    }
    "getBinary" - {
      val mem = Memory(knownRanges = SortedMap(
        MemRange(0, 4) -> BinaryConstant("01234567"),
        MemRange(4, 8) -> Constant(0xabcd),
        MemRange(8, 12) -> DefenderControlledAddress,
        MemRange(12, 14) -> Constant(1),
        MemRange(16, 20) -> BinaryConstant("89abcdef")
      ))
      "should create a byte array from the memory ranges" in {
        printHexBinary(mem.getBinary(2, 16)) shouldEqual "45670000ABCD000000000001000089AB"
      }
    }
    "getSingleValueFromRange" - {
      val mem = Memory(knownRanges = SortedMap(
        MemRange(28, 32) -> AttackerControlled,
        MemRange(32, 36) -> DefenderControlledData
      ))
      "should combine any data points in its memory range" in {
        mem.getSingleValueFromRange(4, 32) shouldEqual (AttackerControlled * (BigInt(1) << 32) + DefenderControlledData)
      }
    }
    "updateRange" - {
      val mem = Memory(knownRanges = SortedMap(
        MemRange(0, 4) -> BinaryConstant("00010203"),
        MemRange(4, 8) -> BinaryConstant("04050607"),
        MemRange(8, 12) -> BinaryConstant("08090a0b"),
        MemRange(12, 16) -> BinaryConstant("0c0d0e0f"),
        MemRange(16, 20) -> BinaryConstant("10111213")
      ))

      "should find ranges that cross boundaries" in {
        mem.putRange(6, 9, SortedMap(
          MemRange(0, 4) -> BinaryConstant("ffffffff"),
          MemRange(5, 9) -> BinaryConstant("eeeeeeee")
        )) shouldEqual Memory(SortedMap(
          MemRange(0, 4) -> BinaryConstant("00010203"),
          MemRange(4, 6) -> BinaryConstant("0405"),
          MemRange(6, 10) -> BinaryConstant("ffffffff"),
          MemRange(11, 15) -> BinaryConstant("eeeeeeee"),
          MemRange(15, 16) -> BinaryConstant("0f"),
          MemRange(16, 20) -> BinaryConstant("10111213")
        ))
      }
      "should find ranges at boundaries" in {
        mem.putRange(8, 8, SortedMap(
          MemRange(0, 4) -> BinaryConstant("ffffffff"),
          MemRange(4, 8) -> BinaryConstant("eeeeeeee")
        )) shouldEqual Memory(SortedMap(
          MemRange(0, 4) -> BinaryConstant("00010203"),
          MemRange(4, 8) -> BinaryConstant("04050607"),
          MemRange(8, 12) -> BinaryConstant("ffffffff"),
          MemRange(12, 16) -> BinaryConstant("eeeeeeee"),
          MemRange(16, 20) -> BinaryConstant("10111213")
        ))
      }
    }
  }
}
