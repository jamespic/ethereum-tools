package io.github.jamespic.ethereum_tools.static_analysis

import javax.xml.bind.DatatypeConverter.parseHexBinary

import io.github.jamespic.ethereum_tools.static_analysis.Execution.Contract
import io.github.jamespic.ethereum_tools.static_analysis.StaticAnalysis.analyseContract
import io.github.jamespic.ethereum_tools.static_analysis.listeners.{LineNumberListener, MultiListener, SentMoneyListener}

object AnalyseParityWallet extends App {
  val listener = MultiListener(
    Map(
      "money" -> SentMoneyListener()
    )
  )

  for ((interest, state) <- analyseContract(0xdeadbeefL, parityWalletContracts, listener).interestingResults) {
    println(s"Interest: $interest, When:\n${state.context}")
  }
}
