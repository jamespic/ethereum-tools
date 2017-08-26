package io.github.jamespic.ethereum_tools.decompiler

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets.UTF_8
import javax.xml.bind.DatatypeConverter

import org.ethereum.util.blockchain.StandaloneBlockchain
import io.github.jamespic.ethereum_tools.Bytecode
import io.github.jamespic.ethereum_tools.decompiler.control_flow.{Block, ControlGraph, Func, GraphRewriteRules}
import io.github.jamespic.ethereum_tools.decompiler.data_flow.AST


object Decompiler {
  def main(args: Array[String]) = {
    val bytecode = args match {
      case Array("--solidity", x, name) =>
        val code = new String(Files.readAllBytes(Paths.get(x)), UTF_8)
        val blockchain = new StandaloneBlockchain().withAutoblock(true)
        try {
          val contract = blockchain.submitNewContract(code, name)
          val contractAddress = contract.getAddress()
          blockchain.getBlockchain().getRepository().getCode(contractAddress)
        } finally blockchain.getBlockchain().close()
      case Array("--bytecode", x) =>
        val code = new String(Files.readAllBytes(Paths.get(x)), UTF_8).trim
        DatatypeConverter.parseHexBinary(code)
    }
    println(decompile(bytecode))
  }

  def decompile(bytes: Array[Byte]) = {
    val instructions = Bytecode.parse(bytes)
    val blocks = Block.identifyBasicBlocks(instructions)
    val controlGraph = ControlGraph(blocks)
    val (functions, signatureHints) = Func.splitIntoFunctions(controlGraph)
    AST.funcsToAst(functions, signatureHints).toSeq.sortBy(_.name).mkString("\n")
//    val rewritten = GraphRewriteRules.stripUnreachable(controlGraph).getOrElse(controlGraph)
//    println(Func.splitIntoFunctions(rewritten))
//    rewritten
  }
}
