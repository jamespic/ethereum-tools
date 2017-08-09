package io.github.jamespic.ethereum_tools.decompiler.control_flow

import scala.collection.immutable.SortedSet

case class ControlGraph(blocks: SortedSet[Block]) {
  lazy val parents: Map[Int, Set[Block]] = {
    val exitBlockMapping = for (
      block <- blocks; exitBlock <- exitBlocks(block.exitPoint)
    ) yield (exitBlock, block)
    val foundParents = exitBlockMapping.groupBy(_._1.address).mapValues(kvPairs => kvPairs.map(_._2))
    val defaults = blocks.map(b => b.address -> SortedSet.empty[Block]).toMap
    defaults ++ foundParents
  }

  lazy val blockByAddress: Map[Int, Block] = {
    blocks.map(block => (block.address, block)).toMap
  }

  def exitBlocks(exitPoint: ExitPoint): Set[Block] = exitPoint match {
    case ConstJump(n) => blockByAddress.get(n).toSet
    case StackJump(_)|Halt|Throw|FunctionReturn(_) => Set.empty
    case ConditionalExit(trueExit, falseExit) => exitBlocks(trueExit) ++ exitBlocks(falseExit)
    case CalculatedJump => Set.empty
  }

  override def toString = blocks.mkString("\n")

  object ExitBlock {
    def unapplySeq(exitPoint: ExitPoint): Option[Seq[Block]] = Some(exitBlocks(exitPoint).toSeq)
    def unapplySeq(stateChange: StateChange): Option[Seq[Block]] = unapplySeq(stateChange.exitPoint)
  }
}

object ControlGraph {
  def apply(blocks: Block*): ControlGraph = ControlGraph(blocks.to[SortedSet])
}
