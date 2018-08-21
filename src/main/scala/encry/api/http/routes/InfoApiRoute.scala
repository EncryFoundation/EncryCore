package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.local.miner.Miner.{GetMinerStatus, MinerStatus}
import encry.network.Handshake
import encry.network.peer.PeerManager.ReceivableMessages.GetConnectedPeers
import encry.settings.{Algos, Constants, EncryAppSettings, RESTApiSettings}
import encry.utils.{NetworkTime, NetworkTimeProvider}
import encry.view.EncryViewReadersHolder.{GetReaders, Readers}
import io.circe.Json
import io.circe.syntax._
import scala.concurrent.Future

case class InfoApiRoute(readersHolder: ActorRef,
                        miner: ActorRef,
                        peerManager: ActorRef,
                        appSettings: EncryAppSettings,
                        nodeId: Array[Byte],
                        timeProvider: NetworkTimeProvider)
                       (implicit val context: ActorRefFactory) extends EncryBaseApiRoute {

  override val settings: RESTApiSettings = appSettings.restApi

  private val launchTimeFuture: Future[NetworkTime.Time] = timeProvider.time()

  override val route: Route = (path("info") & get) {
    val minerInfoF: Future[MinerStatus] = getMinerInfo
    val connectedPeersF: Future[Int] = getConnectedPeers
    val readersF: Future[Readers] = (readersHolder ? GetReaders).mapTo[Readers]
    (for {
      minerInfo <- minerInfoF
      connectedPeers <- connectedPeersF
      readers <- readersF
      currentTime <- timeProvider.time()
      launchTime <- launchTimeFuture
      nodeUptime = currentTime - launchTime
    } yield {
      InfoApiRoute.makeInfoJson(nodeId, minerInfo, connectedPeers, readers, getStateType, getNodeName, nodeUptime)
    }).okJson()
  }

  private def getConnectedPeers: Future[Int] = (peerManager ? GetConnectedPeers).mapTo[Seq[Handshake]].map(_.size)

  private def getStateType: String = appSettings.node.stateMode.verboseName

  private def getNodeName: String = appSettings.network.nodeName

  private def getMinerInfo: Future[MinerStatus] = (miner ? GetMinerStatus).mapTo[MinerStatus]
}

object InfoApiRoute {

  def makeInfoJson(nodeId: Array[Byte],
                   minerInfo: MinerStatus,
                   connectedPeersLength: Int,
                   readers: Readers,
                   stateType: String,
                   nodeName: String,
                   nodeUptime: Long): Json = {
    val stateVersion = readers.s.map(_.version).map(Algos.encode)
    val bestHeader = readers.h.flatMap(_.bestHeaderOpt)
    val bestFullBlock = readers.h.flatMap(_.bestBlockOpt)
    val unconfirmedCount = readers.m.map(_.size).getOrElse(0)
    Map(
      "name" -> nodeName.asJson,
      "headersHeight" -> bestHeader.map(_.height).getOrElse(0).asJson,
      "fullHeight" -> bestFullBlock.map(_.header.height).getOrElse(0).asJson,
      "bestHeaderId" -> bestHeader.map(_.encodedId).asJson,
      "bestFullHeaderId" -> bestFullBlock.map(_.header.encodedId).asJson,
      "previousFullHeaderId" -> bestFullBlock.map(_.header.parentId).map(Algos.encode).asJson,
      "difficulty" -> bestFullBlock.map(block => block.header.difficulty.toString)
        .getOrElse(Constants.Chain.InitialDifficulty.toString).asJson,
      "unconfirmedCount" -> unconfirmedCount.asJson,
      "stateType" -> stateType.asJson,
      "stateVersion" -> stateVersion.asJson,
      "isMining" -> minerInfo.isMining.asJson,
      "peersCount" -> connectedPeersLength.asJson,
      "uptime" -> nodeUptime.asJson
    ).asJson
  }
}
