package io.github.jamespic.ethereum_tools.static_analysis

import java.util.concurrent.ConcurrentHashMap
import javax.xml.bind.DatatypeConverter.parseHexBinary

import akka.actor._
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import io.github.jamespic.ethereum_tools.Bytecode
import io.github.jamespic.ethereum_tools.static_analysis.Execution._
import org.web3j.protocol.Web3jService

import scala.io.{Source, StdIn}
import scala.xml.NodeSeq
import scala.xml.dtd.DocType

case class StateResult(state: ExecutionState, nextStateIds: Seq[Int])

class StateCache(contracts: Map[EVMData, Contract]) {
  val stateCache = new ConcurrentHashMap[Int, ExecutionState]
  val backReferences = new ConcurrentHashMap[Int, Int]

  def startDebugging(contractAddress: BigInt) = {
    val state = attackState(Constant(contractAddress), contracts, 0, maxCalls = 3)
    stateCache.put(getId(state), state)
    getId(state)
  }

  def getState(stateId: Int): StateResult = {
    val state = stateCache.get(stateId)
    val nextStates = state match {
      case x: NonFinalExecutionState =>
        for (nextState <- state.asInstanceOf[NonFinalExecutionState].nextStates) yield {
          stateCache.put(getId(nextState), nextState)
          backReferences.put(getId(nextState), stateId)
          getId(nextState)
        }
      case _: FinishedState => Nil
    }
    StateResult(state, nextStates)
  }

  def nextChoice(stateId: Int): Int = {
    val result = getState(stateId)
    if (result.nextStateIds.size == 1) nextChoice(result.nextStateIds.head)
    else getId(result.state)
  }

  def back(stateId: Int) = {
    if (backReferences.contains(stateId)) Some(backReferences.get(stateId))
    else None
  }
  private def getId(state: ExecutionState) = state.hashCode.abs
}

class WebDebugger(contracts: Map[EVMData, Contract]) {
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher
  implicit val htmlMarshaller: ToEntityMarshaller[NodeSeq] =
    ScalaXmlSupport.nodeSeqMarshaller(MediaTypes.`text/html`).map(
      entity => entity.transformDataBytes(Flow.fromFunction(bytes =>
        ByteString(DocType("html").toString) ++ bytes
      ))
    )
  val cache = new StateCache(contracts)

  val route =
    path("contract" / Segment) {contractId =>
      val hexContractId = if (contractId.startsWith("0x")) contractId.substring(2) else contractId
      val contract = BigInt(hexContractId, 16)
      val startState = cache.startDebugging(contract)
      redirect(Uri(path = getPath(startState)), StatusCodes.Found)
    } ~
    (path("nextChoice"/ IntNumber) & parameterMap) {(stateId, params) =>
      val jumpDest = cache.nextChoice(stateId)
      redirect(Uri(path = getPath(jumpDest)).withQuery(Query(params)), StatusCodes.Found)
    } ~
    (path("state" / IntNumber) & parameterMap) {(stateId, params) =>
      val StateResult(state, nextStates) = cache.getState(stateId)
      complete {
        <html>
          <head>
            <title>Debugging state {state.hashCode.abs}</title>
            <link rel="stylesheet"
                  href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css"
                  integrity="sha384-PsH8R72JQ3SOdhVi3uxftmaW6Vc51MKb0q5P2rRUpPvrszuE4W1povHYgTpBfshb" crossorigin="anonymous" />
            <link rel="stylesheet"
                  href="https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css" />
            <style>
              #code-container {{
                overflow-y: scroll;
                height: 350px;
              }}

              div.active-code {{
                background-color: #aaf;
              }}
            </style>
            <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
          </head>
          <body>
            <div class="container-fluid">
              {renderNavBar(stateId, nextStates)}
              {renderExecutionState(state)}
            </div>

            <script src="https://code.jquery.com/jquery-3.2.1.slim.min.js"
                    integrity="sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN"
                    crossorigin="anonymous"></script>
            <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.3/umd/popper.min.js"
                    integrity="sha384-vFJXuSJphROIrBnz7yo7oB41mKfc8JzQZiCq4NCceLEaO4IHwicKwpJf9c9IpFgh"
                    crossorigin="anonymous"></script>
            <script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/js/bootstrap.min.js"
                    integrity="sha384-alpBpkh1PFOepccYVYDB4do5UnbKysX5WZXm3XxPqe5iKTfUKjNkCk9SaVuEZflJ"
                    crossorigin="anonymous"></script>
          </body>


        </html>
      }
    }

  private def renderNavBar(stateId: Int, nextStates: Seq[Int]) = {
    <div>
      {
      cache.back(stateId) match {
        case Some(backId) =>
          <a class="btn btn-lg btn-info"  accesskey="b" href={getPath(backId).toString}>
            Step Back
            <i class="fa fa-step-backward" aria-hidden="true"></i>
          </a>
        case None => NodeSeq.Empty
      }
      }
      {
      nextStates.size match {
        case 0 => NodeSeq.Empty
        case 1 =>
          val nextState = nextStates.head
          <span>
            <a class="btn btn-lg btn-primary" accesskey="n" href={getPath(nextState).toString}>
              Step Next
              <i class="fa fa-step-forward" aria-hidden="true"></i>
            </a>
            <a class="btn btn-lg btn-danger"  accesskey="c" href={nextChoice(nextState).toString}>
              Step Next Choice
              <i class="fa fa-fast-forward" aria-hidden="true"></i>
            </a>
          </span>
        case _ =>
          <span>
            {
            nextStates flatMap {nextState =>
              val desc = describeNewConstraints(
                cache.getState(stateId).state,
                cache.getState(nextState).state
              )
              Seq(
                <a class="btn btn-lg btn-primary" href={getPath(nextState).toString}>
                  Step Next ({desc})
                  <i class="fa fa-step-forward" aria-hidden="true"></i>
                </a>,
                " "
              )
            }
            }
          </span>
      }
      }
    </div>
  }

  def renderMemory(memory: MemoryLike) = memory match {
    case Memory(zones) =>
      <ul>
        {
          for ((offset, data) <- zones) yield {
            <li>{offset}</li>
            <ul>
              {
                for ((MemRange(start, end), item) <- data.knownRanges) yield
                  <li>
                    <strong>0x{start.toHexString} - 0x{end.toHexString}</strong>:
                    {item}
                  </li>
              }
            </ul>
          }
        }
      </ul>
    case x => <div>{x.toString}</div> // CallData and BinaryConstant have toString methods that are probably adequate
  }

  private def renderExecutionState(state: ExecutionState, breadcrumbs: Seq[String] = Nil): NodeSeq = state match {
    case state: RunningState =>
      <div>{renderBreadcrumbs(breadcrumbs :+ state.address.toString)}</div>
      <div class="row">
        <div class="col-sm-6">
          <h3>Code</h3>
          {renderCode(state.code.binary, state.instructionPointer)}
        </div>
        <div class="col-sm-6">
          <h3>Stack</h3>
          <ul>
            {state.stack.map(x => <li>{x.toString}</li>)}
          </ul>
        </div>
      </div>
      <div class="row">
        <div class="col-sm-4">
          <h3>Constraints</h3>
          {renderConstraints(state)}
        </div>
        <div class="col-sm-4">
          <h3>Storage</h3>
          {renderStorage(state.contract.storage)}
        </div>
        <div class="col-sm-4">
          <h3>Memory</h3>
          {renderMemory(state.memory)}
        </div>
      </div>

    case state: FinishedState =>
        <div>{renderBreadcrumbs(breadcrumbs :+ "Return Data")}</div>
        <p><strong>Success:</strong> {state.success}</p>
        <h2>Result</h2>
        <dl class="row">
          {renderMemory(state.result)}
        </dl>
        <h2>Constraints</h2> :+
        renderConstraints(state)
    case state: AttackerContractState =>
      renderExecutionState(state.calledState, breadcrumbs :+ "Attacker Contract")
    case state: ContractCallState =>
      renderExecutionState(state.calledState, breadcrumbs :+ state.returnState.address.toString)
  }

  def renderStorage(storage: Map[EVMData, EVMData]) = {
    <dl class="row">
      {for ((k, v) <- storage) yield
        <dt class="col-sm-6 text-truncate" title={k.toString}>{k}</dt>
          <dd class="col-sm-6 text-truncate" title={v.toString}>{v}</dd>
      }
    </dl>
  }

  private def renderBreadcrumbs(breadcrumbs: Seq[String]) = {
    <nav aria-label="breadcrumb" role="navigation">
      <ol class="breadcrumb">
        {breadcrumbs map (crumb => <li class="breadcrumb-item">{crumb}</li>)}
      </ol>
    </nav>
  }

  private def renderConstraints(state: ExecutionState) = {
    <ul>
      {
        constraints(state).map(constraint => <li>{constraint}</li>)
      }
    </ul>
  }

  private def renderCode(code: Array[Byte], ip: Int) = {
    val parsed = Bytecode.parse(code)
    <div id="code-container">
      {

        for ((i, byteCode) <- parsed) yield {
          val clazz = if (ip == i) "active-code" else ""
          <div class={clazz}>{f"$i%04x"} {byteCode}</div>
        }
      }
    </div>
    <script>
      var selected = document.querySelector('.active-code')
      var parent =document.getElementById('code-container')
      parent.scrollTop = selected.offsetTop - parent.offsetTop
    </script>
  }

  private def describeNewConstraints(oldState: ExecutionState, newState: ExecutionState) = {
    (constraints(newState) -- constraints(oldState)).mkString(", ")
  }

  private def constraints(state: ExecutionState): Set[String] = state match {
    case x: FinishedState => x.context.constraints.toString.split("\n").filter(_ != "").toSet
    case x: AttackerContractState => constraints(x.calledState)
    case x: ContractCallState => constraints(x.calledState)
    case x: RunningState => x.context.constraints.toString.split("\n").filter(_ != "").toSet
  }

  private def getPath(startState: Int) = {
    Uri.Path / "state" / startState.toString
  }

  private def nextChoice(state: Int) = {
    Uri.Path / "nextChoice" / state.toString
  }

  def run() = {
    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}

object WebDebugger extends App {
  val contracts = this.args match {
    case Array("parityWallet") => parityWalletContracts
    case Array(filename) =>
      val contractCode = parseHexBinary(Source.fromFile(filename).getLines().mkString(""))
      val simpleContract = Contract(Memory(contractCode), Map.empty, Constant(0xfeed))
      Map[EVMData, Contract](Constant(0xfeed) -> simpleContract)
    case _ => BlockchainContracts.default.ContractMap()
  }

  new WebDebugger(contracts).run()
}