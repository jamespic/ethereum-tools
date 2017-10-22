package io.github.jamespic.ethereum_tools.static_analysis

import scala.io.Source
import javax.xml.bind.DatatypeConverter.parseHexBinary

import io.github.jamespic.ethereum_tools.static_analysis.Execution.Contract
import io.github.jamespic.ethereum_tools.static_analysis.StaticAnalysis.analyseContract
import io.github.jamespic.ethereum_tools.static_analysis.listeners.{HistoryListener, MultiListener, SentMoneyListener}

object CliApp extends App {
  val contractCode = this.args match {
    case Array(filename) =>
      parseHexBinary(Source.fromFile(filename).getLines().mkString(""))
    case _ => parseHexBinary(
      "6000" // PUSH 0 - out length
        + "6000" // PUSH 0 - out offset
        + "6000" // PUSH 0 - in length
        + "6000" // PUSH 0 - in offset
        + "6001303103" // PUSH 1 ADDRESS BALANCE SUB - value
        + "33" // CALLER - to
        + "62100000" // PUSH 100000 - gas
        + "f1" // CALL
    )
  }
  val simpleContract = Contract(Memory(contractCode), Map.empty, Constant(1000000))
  val address = BigInt(1000000)
  val contracts = Map[EVMData, Contract](Constant(address) -> simpleContract)
  val listener = SentMoneyListener()
  for ((interest, state) <- analyseContract(address, contracts, listener)) {
    println(s"Interest: $interest, When:\n${state.context}")
  }
}
