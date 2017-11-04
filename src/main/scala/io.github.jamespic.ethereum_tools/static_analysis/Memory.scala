package io.github.jamespic.ethereum_tools.static_analysis

import scala.collection.SortedMap

case class MemRange(start: Int, end: Int) extends Ordered[MemRange] with HashMemo {
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
  def apply(binary: Array[Byte]): MemoryLike = {
    val memConstant = BinaryConstant(binary)
    Memory(
      SortedMap(
        MemRange(0, binary.length) -> memConstant
      )
    )
  }

  def apply(knownRanges: SortedMap[MemRange, EVMData] = SortedMap.empty) =
    new Memory(Map(Constant(0) -> MemoryZone(knownRanges)))
  def apply(knownRanges: (MemRange, EVMData)*) =
    new Memory(Map(Constant(0) -> MemoryZone(SortedMap(knownRanges: _*))))
}
case class Memory(zones: Map[EVMData, MemoryZone]) extends HashMemo with MemoryLike {
  private def getZoneStartAndOffset(start: EVMData) = start match {
    case Constant(n) => (Constant(0), n.toInt)
    case AddExpr(x, Constant(o)) => (x, o.toInt)
    case x => (x, 0)
  }

  override def getRange(start: EVMData, length: EVMData): SortedMap[MemRange, EVMData] = {
    val (zoneKey, offset) = getZoneStartAndOffset(start)
    zones.get(zoneKey) match {
      case Some(zone) =>
        length match {
          case Constant(l) => zone.getRange(offset, l.toInt)
          case _ => zone.getRange(offset)
        }
      case None => SortedMap.empty
    }
  }

  override def slice(start: EVMData, length: EVMData): MemoryLike = {
    val range = getRange(start, length)
    if (range.size == 1 && range.head._1.start == 0 && range.head._2.isInstanceOf[MemoryLike]){
      range.head._2.asInstanceOf[MemoryLike]
    } else Memory(range)
  }

  override def get(start: EVMData, len: EVMData = Constant(32)) = {
    val (zoneKey, offset) = getZoneStartAndOffset(start)
    zones.get(zoneKey) match {
      case Some(zone) =>
        len match {
          case Constant(l) => zone.get(offset, l.toInt)
          case _ => zone.get(offset)
        }
      case None => Constant(0)
    }
  }

  override def put(key: EVMData, value: EVMData): Memory = {
    val (zoneKey, offset) = getZoneStartAndOffset(key)
    zones.get(zoneKey) match {
      case Some(zone) =>
        copy(zones = zones.updated(zoneKey, zone.putRange(offset, 32, SortedMap(MemRange(0, 32) -> value))))
      case None =>
        copy(zones = zones.updated(zoneKey, MemoryZone(SortedMap(MemRange(0, 32) -> value))))
    }
  }

  override def putRange(start: EVMData, length: EVMData, data: Iterable[(MemRange, EVMData)]): Memory = {
    val (zoneKey, offset) = getZoneStartAndOffset(start)
    zones.get(zoneKey) match {
      case Some(zone) =>
        length match {
          case Constant(l) => copy(zones = zones.updated(zoneKey, zone.putRange(offset, l.toInt, data)))
          case _ => copy(zones = zones.updated(zoneKey, zone.putRange(offset, data)))
        }
      case None =>
        copy(zones = zones.updated(zoneKey, MemoryZone(SortedMap(data.toSeq: _*))))
    }
  }

  override def getBinary(start: EVMData, length: EVMData): Array[Byte] = {
    val (zoneKey, offset) = getZoneStartAndOffset(start)
    zones.get(zoneKey) match {
      case Some(zone) =>
        length match {
          case Constant(l) => zone.getBinary(offset, l.toInt)
          case _ => zone.binary.drop(offset)
        }
      case None =>
        length match {
          case Constant(n) => new Array[Byte](n.toInt)
          case _ => new Array[Byte](0)
        }
    }
  }

  lazy val binary = {
    zones.get(0).fold(new Array[Byte](0))(_.binary)
  }
}

case class MemoryZone(knownRanges: SortedMap[MemRange, EVMData] = SortedMap.empty) extends HashMemo {
  private[static_analysis] def intersectingRanges(start: Int, end: Int): SortedMap[MemRange, EVMData] = {
    val firstRange = knownRanges.until(MemRange(start, start)).lastOption filter {
      case (MemRange(_, rangeEnd), _) => rangeEnd > start
    }
    val restRange = knownRanges.from(MemRange(start, start)).until(MemRange(end, end))
    restRange ++ firstRange
  }

  private[static_analysis] def intersectingRanges(start: Int): SortedMap[MemRange, EVMData] = {
    val firstRange = knownRanges.until(MemRange(start, start)).lastOption filter {
      case (MemRange(_, rangeEnd), _) => rangeEnd > start
    }
    val restRange = knownRanges.from(MemRange(start, start))
    restRange ++ firstRange
  }

  def getRange(start: Int, length: Int): SortedMap[MemRange, EVMData] = {
    val end = start + length
    val range = intersectingRanges(start, end)
    for ((MemRange(segStart, segEnd), x) <- range) yield {
      val newStart = start max segStart
      val newEnd = end min segEnd
      val endClipped = if (segEnd > end) x.clipLowBytes(segEnd - end) else x
      val fullyClipped = if (segStart < start) endClipped.clipHighBytes(newEnd - start) else endClipped
      MemRange(newStart - start, newEnd - start) -> fullyClipped
    }
  }

  def getRange(start: Int): SortedMap[MemRange, EVMData] = {
    val range = intersectingRanges(start)
    for ((MemRange(segStart, segEnd), x) <- range) yield {
      val newStart = start max segStart
      val clipped = if (segStart < start) x.clipHighBytes(segEnd - start) else x
      MemRange(newStart - start, segEnd - start) -> clipped
    }
  }

  def get(start: Int, length: Int): EVMData = {
    ((Constant(0):EVMData) /: getRange(start, length)){
      case (x, (MemRange(_, end), y)) => x +! y * (BigInt(1) << (8 * (length - end)))
    }
  }

  def get(start: Int): EVMData = get(start, 32) // Most likely use is getting a single word


  def putRange(start: Int, length: Int, newData: Iterable[(MemRange, EVMData)]): MemoryZone = {
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

  def putRange(start: Int, newData: Iterable[(MemRange, EVMData)]): MemoryZone = {
    // Check everything in range
    assert((true /: newData){case (a, (MemRange(s, e), _)) => a && 0 <= s && s <= e})
    val damagedRanges = intersectingRanges(start)
    val startRangeItem = for ((MemRange(s, e), x) <- damagedRanges.headOption if s < start) yield {
      MemRange(s, start) -> x.clipLowBytes(e - start)
    }
    val newRanges = knownRanges -- damagedRanges.keys ++ startRangeItem ++
      (for ((MemRange(s, e), x) <- newData) yield MemRange(s + start, e + start) -> x)
    copy(knownRanges = newRanges)
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

  lazy val binary = {
    val length = knownRanges.lastOption match {
      case Some((MemRange(_, end), _)) => end
      case None => 0
    }
    getBinary(0, length)
  }
}