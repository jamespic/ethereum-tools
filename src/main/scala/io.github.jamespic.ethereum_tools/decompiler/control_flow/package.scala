package io.github.jamespic.ethereum_tools.decompiler

import io.github.jamespic.ethereum_tools._
import Bytecode._


package object control_flow {
  def identifyBasicBlocks(instructions: InstList): Map[Int, InstList] = {
    val result = Map.newBuilder[Int, InstList]
    var currentBlock = List.newBuilder[(Int, Bytecode)]
    var blockStart = 0
    instructions foreach {
      case x @ (i, STOP|JUMP|JUMPI|RETURN|INVALID|REVERT|SUICIDE|UNKNOWN) =>
        currentBlock += x
        result += blockStart -> currentBlock.result()
        blockStart = i + 1  // Luckily these are all one byte ops
        currentBlock = List.newBuilder[(Int, Bytecode)]
      case x =>
        currentBlock += x
    }
    result.result()
  }
}
