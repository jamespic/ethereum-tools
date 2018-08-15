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
  def latest = {
    val web3jService = new HttpService()
    val web3 = Web3j.build(web3jService)
    new BlockchainContracts(web3jService, web3.ethBlockNumber().send().getBlockNumber.longValue)
  }
  def forBlock(blockNumber: Long): BlockchainContracts = {
    val web3jService = new HttpService()
    val web3 = Web3j.build(web3jService)
    new BlockchainContracts(web3jService, blockNumber)
  }
  def forBlock(blockNumber: Option[Long]): BlockchainContracts = blockNumber match {
    case Some(n) => forBlock(n)
    case None => latest
  }
}

class BlockchainContracts(web3Service: Web3jService, blockNumber: Long) {
  var web3 = Web3j.build(web3Service)
  var cache = MMap.empty[EVMData, Option[Contract]]
  val block = new DefaultBlockParameterNumber(blockNumber)

  private def lookup(address: EVMData): Option[Contract] = {
    cache.get(address) match {
      case Some(x) => x
      case None =>
        val result = address match {
          case Constant(n) =>
            val address = f"0x$n%040x"
            val codeString = web3.ethGetCode(address, block).send().getCode
            val code = if (codeString != null) parseHexBinary(codeString.substring(2)) else new Array[Byte](0)
            val balance = web3.ethGetBalance(address, block).send().getBalance
            val nonce = web3.ethGetTransactionCount(address, block).send().getTransactionCount.intValue()
            val storage = getStorageMap(address)
            Some(Contract(BinaryConstant(code), storage, Constant(balance), nonce))
          case _ => None
        }
        cache(address) = result
        result
    }

  }

  protected def getStorageMap(address: String): Map[EVMData, EVMData] = {
    (for (key <- getStorageKeys(address, block)) yield {
      val keyBigInt = decodeQuantity(key)
      val valueBigInt = decodeQuantity(web3.ethGetStorageAt(address, keyBigInt, block).send().getData)
      Constant(keyBigInt) -> Constant(valueBigInt)
    }).toMap[EVMData, EVMData]
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
    override def iterator: Iterator[(EVMData, Contract)] = (overridden -- tombstones).iterator
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

object LazyBlockchainContracts {
  def latest = {
    val web3jService = new HttpService()
    val web3 = Web3j.build(web3jService)
    new LazyBlockchainContracts(web3jService, web3.ethBlockNumber().send().getBlockNumber.longValue)
  }
  def forBlock(blockNumber: Long): BlockchainContracts = {
    val web3jService = new HttpService()
    val web3 = Web3j.build(web3jService)
    new LazyBlockchainContracts(web3jService, blockNumber)
  }
  def forBlock(blockNumber: Option[Long]): BlockchainContracts = blockNumber match {
    case Some(n) => forBlock(n)
    case None => latest
  }
}

class LazyBlockchainContracts(web3Service: Web3jService, blockNumber: Long) extends BlockchainContracts(web3Service, blockNumber) {
  val storageCache = MMap.empty[(String, BigInt), EVMData]
  protected override def getStorageMap(address: String): Map[EVMData, EVMData] = {
    val underlying = (for (key <- 0 to 64) yield {
      val keyBigInt = java.math.BigInteger.valueOf(key)
      val valueBigInt = decodeQuantity(web3.ethGetStorageAt(address, keyBigInt, block).send().getData)
      Constant(keyBigInt) -> Constant(valueBigInt)
    }).toMap[EVMData, EVMData].filter(_._2 != Constant(0))
    return new LazyStorageMap(address, underlying, Set.empty[EVMData])
  }
  private class LazyStorageMap(address: String, val overridden: Map[EVMData, EVMData], tombstones: Set[EVMData]) extends Map[EVMData, EVMData] {
    override def +[B1 >: EVMData](kv: (EVMData, B1)): Map[EVMData, B1] = kv match {
      case (k, v: EVMData) => new LazyStorageMap(address, overridden.updated(k, v), tombstones - k)
    }
    override def get(key: EVMData): Option[EVMData] =
      overridden.get(key).orElse(lookup(key)).filter(x => !tombstones.contains(x))
    override def iterator: Iterator[(EVMData, EVMData)] = (overridden -- tombstones).iterator
    override def -(key: EVMData): Map[EVMData, EVMData] = new LazyStorageMap(address, overridden - key, tombstones + key)
    override lazy val hashCode = super.hashCode
    private def lookup(key: EVMData): Option[EVMData] = key match {
      case Constant(x) =>
        storageCache.get((address, x)).orElse {
          val storageValue = BigInt(decodeQuantity(web3.ethGetStorageAt(address, x.bigInteger, block).send().getData))
          val result = Constant(storageValue)
          storageCache((address, x)) = Constant(storageValue)
          Some(result)
        }
      case _ => None
    }
  }
}
