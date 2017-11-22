package io.github.jamespic.ethereum_tools.static_analysis

import scala.collection.mutable.{Map => MMap}
import scala.collection.JavaConversions._
import java.util.{List => JList}
import javax.xml.bind.DatatypeConverter.parseHexBinary

import org.web3j.utils.Numeric.decodeQuantity
import org.web3j.protocol.{Web3j, Web3jService}
import org.web3j.protocol.core.{DefaultBlockParameter, DefaultBlockParameterNumber, Request, Response}
import io.github.jamespic.ethereum_tools.static_analysis.Execution.Contract
import org.web3j.protocol.http.HttpService

import scala.collection.generic.{CanBuildFrom, MapFactory}
import scala.collection.mutable

object BlockchainContracts {
  def default = {
    val web3jService = new HttpService()
    val web3 = Web3j.build(web3jService)
    new BlockchainContracts(web3jService, web3.ethBlockNumber().send().getBlockNumber.longValue)
  }
}

class BlockchainContracts(web3Service: Web3jService, blockNumber: Long) {
  var web3 = Web3j.build(web3Service)
  var cache = MMap.empty[EVMData, Option[Contract]]

  private def lookup(address: EVMData): Option[Contract] = {
    cache.get(address) match {
      case Some(x) => x
      case None =>
        val result = address match {
          case Constant(n) =>
            val block = new DefaultBlockParameterNumber(blockNumber)
            val address = f"0x$n%040x"
            val codeString = web3.ethGetCode(address, block).send().getCode
            val code = if (codeString != null) parseHexBinary(codeString.substring(2)) else new Array[Byte](0)
            val balance = web3.ethGetBalance(address, block).send().getBalance
            val nonce = web3.ethGetTransactionCount(address, block).send().getTransactionCount.intValue()
            val storage = (for (key <- getStorageKeys(address, block)) yield {
              val keyBigInt = decodeQuantity(key)
              val valueBigInt = decodeQuantity(web3.ethGetStorageAt(address, keyBigInt, block).send().getData)
              Constant(keyBigInt) -> Constant(valueBigInt)
            }).toMap[EVMData, EVMData]
            Some(Contract(BinaryConstant(code), storage, Constant(balance), nonce))
          case _ => None
        }
        cache(address) = result
        result
    }

  }

  private def getStorageKeys(address: String, block: DefaultBlockParameter) = {
    val result = Seq.newBuilder[String]
    val request = new Request[Any, Response[JList[String]]](
      "parity_listStorageKeys",
      List(address, 1000, null, block),
      1,
      web3Service,
      classOf[Response[JList[String]]]
    )
    var response = request.send().getResult
    if (response == null) response = Nil
    result ++= response
    while (response.nonEmpty) {
      val request = new Request[Any, Response[JList[String]]](
        "parity_listStorageKeys",
        List(address, 1000, response.last, block),
        1,
        web3Service,
        classOf[Response[JList[String]]]
      )
      response = request.send().getResult
      if (response == null) response = Nil
      result ++= response
    }
    result.result()
  }




  class ContractMap private(val overridden: Map[EVMData, Contract], tombstones: Set[EVMData]) extends Map[EVMData, Contract] {
    override def +[B1 >: Contract](kv: (EVMData, B1)): Map[EVMData, B1] = kv match {
      case (k, v: Contract) => new ContractMap(overridden.updated(k, v), tombstones - k)
    }
    override def get(key: EVMData): Option[Contract] =
      overridden.get(key).orElse(lookup(key)).filter(x => !tombstones.contains(x))
    override def iterator: Iterator[(EVMData, Contract)] = overridden.iterator
    override def -(key: EVMData): Map[EVMData, Contract] = new ContractMap(overridden - key, tombstones + key)
    override lazy val hashCode = super.hashCode
  }

  object ContractMap {
    def empty: ContractMap = new ContractMap(Map.empty, Set.empty)

    def newBuilder = new mutable.Builder[(EVMData, Contract), ContractMap] {
      private var delegate = Map.newBuilder[EVMData, Contract]
      override def +=(elem: (EVMData, Contract)) = {
        delegate += elem
        this
      }
      override def clear() = delegate = Map.newBuilder[EVMData, Contract]
      override def result() = new ContractMap(delegate.result(), Set.empty)
    }

    def apply(elems: (EVMData, Contract)*) = new ContractMap(elems.toMap, Set.empty)

    implicit def canBuildFrom = new CanBuildFrom[ContractMap, (EVMData, Contract), ContractMap] {
      override def apply(from: ContractMap): mutable.Builder[(EVMData, Contract), ContractMap] = newBuilder
      override def apply(): mutable.Builder[(EVMData, Contract), ContractMap] = newBuilder
    }
  }

}
