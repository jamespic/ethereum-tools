package io.github.jamespic.ethereum_tools.static_analysis

import scala.collection.SortedMap

case class MemRange(start: Int, end: Int) extends Ordered[MemRange] {
  def length = end - start
  def contains(that: MemRange) = this.start <= that.start && that.end <= this.end
  def intersection(that: MemRange) = {
    val newStart = this.start max that.start
    val newEnd = this.end min that.end
    if (newStart < newEnd) Some(MemRange(newStart, newEnd))
    else None
  }

  override def compare(that: MemRange): Int = this.start compareTo that.start
}
case object Memory {
  def apply(binary: Array[Byte]): Memory = {
    val memConstant = BinaryConstant(binary)
    Memory(
      SortedMap(
        MemRange(0, binary.length) -> memConstant
      ),
      Map(Constant(0) -> memConstant)
    )
  }
}
case class Memory(knownRanges: SortedMap[MemRange, EVMData] = SortedMap.empty,
                  knownValues: Map[EVMData, EVMData] = Map.empty) {
  private[static_analysis] def intersectingRanges(start: Int, end: Int) = {
    val firstRange = knownRanges.until(MemRange(start, start)).lastOption filter {
      case (MemRange(_, rangeEnd), _) => rangeEnd > start
    }
    val restRange = knownRanges.from(MemRange(start, start)).until(MemRange(end, end))
    restRange ++ firstRange
  }

  def getRange(start: Int, length: Int): SortedMap[MemRange, EVMData] = {
    val end = start + length
    val range = intersectingRanges(start, end)
    for ((MemRange(segStart, segEnd), x) <- range) yield {
      val endClipped = if (segEnd > end) x.clipLowBytes(end - segStart) else x
      val fullyClipped = if (segStart < start) endClipped.clipHighBytes(start - segStart) else endClipped
      MemRange((start max segStart) - start, (end min segEnd) - start) -> fullyClipped
    }
  }

  def getRange(start: EVMData, length: EVMData): SortedMap[MemRange, EVMData] = (start, length) match {
    case (Constant(s), Constant(l)) => getRange(s, l)
    case (x, Constant(l)) => SortedMap((for (i <- 0 until l.toInt) yield MemRange(i, i + 32) -> get(i)): _*)
    case _ => SortedMap()
  }

  def getBinary(start: Int, length: Int) = {
    val result = new Array[Byte](length)
    for ((MemRange(segStart, segEnd), x) <- getRange(start, length)) {
      x match {
        case x: BinaryConstant => x.binData.copyToArray(result, segStart, segEnd - segStart)
        case Constant(n) =>
          val data = n.toByteArray
          val (s, len) = if (data.length < segEnd - segStart)
            (segEnd - data.length, data.length) else (segStart, segEnd - segStart)
          data.copyToArray(result, s, len)
        case _ => // Do nothing - maybe warn??
      }
    }
    result
  }

  def getSingleValueFromRange(start: Int, length: Int) = {
    ((Constant(0):EVMData) /: getRange(start, length)){
      case (x, (MemRange(_, end), y)) => x + y * (BigInt(1) << (8 * (32 - end)))
    }
  }

  def get(key: EVMData): EVMData = {
    key match {
      case Constant(n) =>
        getSingleValueFromRange(n.toInt, n.toInt + 32)
      case _ => knownValues.getOrElse(key, Constant(0))
    }
  }
  def putRange(start: Int, length: Int, newData: Iterable[(MemRange, EVMData)]): Memory = {
    // Check everything in range
    assert((true /: newData){case (a, (MemRange(s, e), _)) => a && 0 <= s && s <= e && e <= length})
    val end = start + length
    val damagedRanges = intersectingRanges(start, end)
    val startRangeItem = for ((MemRange(s, e), x) <- damagedRanges.headOption if s < start) yield {
      MemRange(s, start) -> x.clipLowBytes(e - start)
    }
    val endRangeItem = for ((MemRange(s, e), x) <- damagedRanges.lastOption if e > end) yield {
      MemRange(end, e) -> x.clipHighBytes(e - end)
    }
    val newRanges = knownRanges -- damagedRanges.keys ++ startRangeItem ++ endRangeItem ++
      (for ((MemRange(s, e), x) <- newData) yield MemRange(s + start, e + start) -> x)
    copy(knownRanges = newRanges)
  }

  def put(key: EVMData, value: EVMData) = key match {
    case Constant(n) => putRange(n.toInt, n.toInt + 32, SortedMap(MemRange(0, 32) -> value))
    case _ => copy(knownValues = knownValues + (key -> value))
  }

  def putRange(start: EVMData, length: EVMData, data: Iterable[(MemRange, EVMData)]): Memory = (start, length) match {
    case (Constant(s), Constant(l)) => putRange(s.toInt, l.toInt, data)
    case (s, _) => copy(knownValues = knownValues ++ (for ((MemRange(o, _), value) <- data) yield (s + o) -> value))
  }

  lazy val binary = {
    val length = knownRanges.lastOption match {
      case Some((MemRange(_, end), _)) => end
      case None => 0
    }
    getBinary(0, length)
  }

}
