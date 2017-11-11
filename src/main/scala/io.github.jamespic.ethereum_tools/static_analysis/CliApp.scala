package io.github.jamespic.ethereum_tools.static_analysis

import scala.io.Source
import javax.xml.bind.DatatypeConverter.parseHexBinary

import io.github.jamespic.ethereum_tools.static_analysis.Execution.Contract
import io.github.jamespic.ethereum_tools.static_analysis.StaticAnalysis.analyseContract
import io.github.jamespic.ethereum_tools.static_analysis.listeners.{HistoryListener, LineNumberListener, MultiListener, SentMoneyListener}
import org.web3j.protocol.{Web3j, Web3jService}
import org.web3j.protocol.http.HttpService
import org.web3j.utils.Numeric.decodeQuantity

object CliApp extends App {
  val ContractRe = "(0x[0-9a-fA-F]{40})".r
  val (contracts, address) = args match {
    case Array(ContractRe(addr)) =>
      val blockchain = BlockchainContracts.default
      val contracts = blockchain.ContractMap()
      val address = BigInt(decodeQuantity(addr))
      (contracts, address)
    case Array(filename) =>
      val contractCode = parseHexBinary(Source.fromFile(filename).getLines().mkString(""))
      val simpleContract = Contract(Memory(contractCode), Map.empty, Constant(1000000))
      val address = BigInt(1000000)
      val contracts = Map[EVMData, Contract](Constant(address) -> simpleContract)
      (contracts, address)
  }
  val listener = SentMoneyListener()
  for ((interest, state) <- analyseContract(address, contracts, listener)) {
    println(s"Interest: $interest, When:\n${state.context}")
  }
}
