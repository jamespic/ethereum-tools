package io.github.jamespic

import scala.collection.JavaConversions._
import org.ethereum.util.blockchain.StandaloneBlockchain
import org.ethereum.facade.{EthereumFactory, Repository}

package object ethereum_tools {
  type InstList = List[(Int, Bytecode)]

  def copyContractToTestLab(testLab: StandaloneBlockchain, mainRepo: Repository, address: Array[Byte]) = {
    val testRepo = testLab.getBlockchain.getRepository
    val newAccount = testRepo.createAccount(address)
      .withBalanceIncrement(mainRepo.getBalance(address))
    testRepo.saveCode(address, mainRepo.getCode(address))
    for ((key, value) <- mainRepo.getStorage(address, null)) testRepo.addStorageRow(address, key, value)
    testRepo.commit()
  }

  def copyContractDetailsFromMainNet(address: Array[Byte]) = {
    val ether = EthereumFactory.createEthereum()
    val testLab = new StandaloneBlockchain
    copyContractToTestLab(testLab, ether.getLastRepositorySnapshot, address)
    testLab
  }
}
