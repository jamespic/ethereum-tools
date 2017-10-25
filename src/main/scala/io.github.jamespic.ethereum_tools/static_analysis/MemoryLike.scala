package io.github.jamespic.ethereum_tools.static_analysis

import scala.collection.SortedMap

trait MemoryLike {

  def getRange(start: EVMData, length: EVMData): SortedMap[MemRange, EVMData]

  def slice(start:EVMData, length: EVMData): MemoryLike

  def get(start: EVMData, len: EVMData = Constant(32)): EVMData

  def put(key: EVMData, value: EVMData): MemoryLike

  def putRange(start: EVMData, length: EVMData, data: Iterable[(MemRange, EVMData)]): MemoryLike

  def getBinary(start: EVMData, length: EVMData): Array[Byte]

  val binary: Array[Byte]
}
