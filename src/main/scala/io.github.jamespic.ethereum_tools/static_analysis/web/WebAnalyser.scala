package io.github.jamespic.ethereum_tools.static_analysis.web



import java.util.concurrent.Executors

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives.{path, _}
import io.github.jamespic.ethereum_tools.static_analysis.{BlockchainContracts, StaticAnalysis}
import io.github.jamespic.ethereum_tools.static_analysis.StaticAnalysis.{Interesting, StateListener}
import io.github.jamespic.ethereum_tools.static_analysis.listeners.{MultiListener, SentMoneyListener}

import scala.collection.mutable.{Map => MMap}
import scala.concurrent._
import scala.concurrent.duration.FiniteDuration

class ThreadPoolAnalyser[T](timeLimit: Option[FiniteDuration], listener: StateListener[T]) {
  private case class Request(address: BigInt, blockNumber: Option[Long])
  private val resultCache = MMap.empty[Request, Future[StaticAnalysis.Result[T]]]
  implicit val executionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1))

  def getAnalysis(address: BigInt, blockNumber: Option[Long]) = synchronized {
    val request = Request(address, blockNumber)
    resultCache.get(request) match {
      case Some(f) => f
      case None =>
        val result = Future {
          StaticAnalysis.analyseContract(
            address, BlockchainContracts.forBlock(blockNumber).ContractMap(), listener, timeLimit)
        }
        resultCache(request) = result
        result
    }
  }
}

class WebAnalyser(timeLimit: Option[FiniteDuration]) {
  val analyser = new ThreadPoolAnalyser[Map[String, Any]](timeLimit, MultiListener(
    "money" -> SentMoneyListener()
  ))

  import analyser.executionContext

  val route = (path("analyse" / Segment) & parameterMap) {(address, paramMap) =>
    val blockNumber = paramMap.get("blockNumber") map (_.toLong)
    val hexContractId = if (address.startsWith("0x")) address.substring(2) else address
    val contract = BigInt(hexContractId, 16)
    complete {
      analyser.getAnalysis(contract, blockNumber) map {result =>
        <html>
          <head>
            <title>Analysis of {address}</title>
          </head>
          <body>
            <h1>Analysis of {address}</h1>
            <p><a href={s"/contract/0x$hexContractId"}>Debug this contract</a></p>
            {renderResult(result)}
          </body>
        </html>
      }
    }

  }

  def renderResult(result: StaticAnalysis.Result[Map[String, Any]]) = {
    <p>
      {
        if (!result.completed) "These results are partial, because the analysis ran out of time"
        else "Analysis complete"
      }
    </p>
    <div>
      {
        if (result.interestingResults.isEmpty) <p>This contract is not interesting</p>
        else {
          for ((Interesting(interest), state) <- result.interestingResults) yield {
            <h2>Case</h2>
            <p>{interest}</p>
            <ul>
              {
                for {
                  line <- StaticAnalysis.getContext(state).constraints.toString.split("\n")
                  if line != ""
                } yield <li>{line}</li>
              }
            </ul>
          }
        }
      }
    </div>
  }
}
