package encry.api.http

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.server.ExceptionHandler
import akka.stream.ActorMaterializer
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi._
import encry.api.http.routes._
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedState, NodeViewChange}
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.history.EncryHistoryReader
import encry.view.state.UtxoStateReader
import org.encryfoundation.common.utils.Algos
import encry.EncryApp.memoryPool
import encry.local.miner.Miner.MinerStatus
import encry.network.PeerConnectionHandler.ConnectedPeer
import scala.io.Source

class DataHolderForApi(settings: EncryAppSettings,
                       ntp: NetworkTimeProvider) extends Actor with StrictLogging {

  implicit val system: ActorSystem = context.system
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    //nodeViewHolder ! GetNodeViewChanges(history = true, state = true, vault = true)
  }

  if (settings.restApi.enabled.getOrElse(false)) {
    import akka.http.scaladsl.model.StatusCodes._
    import akka.http.scaladsl.server.Directives._

    val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")
    val nodeId: Array[Byte] = Algos.hash(settings.network.nodeName
      .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)).take(5)

    implicit def apiExceptionHandler: ExceptionHandler = ExceptionHandler {
      case e: Exception => extractUri { uri =>
        logger.info(s"Request to $uri could not be handled normally due to: $e")
        complete(HttpResponse(InternalServerError, entity = "Internal server error"))
      }
    }

    val apiRoutes: Seq[ApiRoute] = Seq(
      UtilsApiRoute(settings.restApi),
      PeersApiRoute(settings.restApi, self)(system),
      InfoApiRoute(self, settings, nodeId, ntp),
      HistoryApiRoute(self, settings, nodeId, settings.node.stateMode),
      TransactionsApiRoute(self, memoryPool,  settings.restApi, settings.node.stateMode),
      StateInfoApiRoute(self, settings.restApi, settings.node.stateMode),
      WalletInfoApiRoute(self, settings.restApi)
    )
    Http().bindAndHandle(
      CompositeHttpService(context.system, apiRoutes, settings.restApi, swaggerConfig).compositeRoute,
      settings.restApi.bindAddress.getAddress.getHostAddress,
      settings.restApi.bindAddress.getPort)
  }

  override def receive: Receive = workingCycle()

  def workingCycle(blackList: Seq[InetAddress] = Seq.empty,
                   connectedPeers: Seq[ConnectedPeer] = Seq.empty,
                   history: Option[EncryHistoryReader] = None,
                   state: Option[UtxoStateReader] = None,
                   transactionsOnMinerActor: Int = 0,
                   minerStatus: MinerStatus = MinerStatus(isMining = false, None)): Receive = {
    case UpdatingTransactionsNumberForApi(qty) =>
      context.become(workingCycle(blackList, connectedPeers, history, state, qty, minerStatus))

    case ChangedHistory(reader: EncryHistoryReader@unchecked) if reader.isInstanceOf[EncryHistoryReader] =>
      context.become(workingCycle(blackList, connectedPeers, Some(reader), state, transactionsOnMinerActor, minerStatus))

    case ChangedState(reader: UtxoStateReader@unchecked) if reader.isInstanceOf[UtxoStateReader] =>
      context.become(workingCycle(blackList, connectedPeers, history, Some(reader), transactionsOnMinerActor, minerStatus))

    case UpdatingMinerStatus(status) =>
      context.become(workingCycle(blackList, connectedPeers, history, state, transactionsOnMinerActor, status))

    case UpdatingConnectedPeers(peers) =>
      context.become(workingCycle(blackList, peers, history, state, transactionsOnMinerActor, minerStatus))

    case GetConnectedPeers => sender() ! connectedPeers
    case GetDataFromHistory => history.foreach(sender() ! _)
    case GetMinerStatus => sender() ! minerStatus
    case GetReaders => sender() ! Readers(history, state)
    case GetTransactionsNumber => sender() ! transactionsOnMinerActor
    case _ =>
  }
}

object DataHolderForApi {

  final case class UpdatingBlackListForApi(blackList: Seq[InetAddress])

  final case class UpdatingConnectedListForApi(connectedPeers: Seq[InetSocketAddress])

  final case class UpdatingTransactionsNumberForApi(number: Int) extends AnyVal

  final case class UpdatingMinerStatus(minerStatus: MinerStatus)

  final case class UpdatingConnectedPeers(peers: Seq[ConnectedPeer])

  case object GetTransactionsNumber

  case object GetReaders

  case object GetConnectedPeers

  case object GetDataFromHistory

  case object GetMinerStatus

  final case class Readers(h: Option[EncryHistoryReader], s: Option[UtxoStateReader])

  def props(settings: EncryAppSettings,
            ntp: NetworkTimeProvider): Props = Props(new DataHolderForApi(settings, ntp))
}