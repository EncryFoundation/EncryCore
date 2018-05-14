package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.consensus.DifficultySerializer
import encry.local.miner.EncryMiner.{GetMinerStatus, MinerStatus}
import encry.settings.{Algos, Constants, EncryAppSettings}
import encry.view.EncryViewReadersHolder.{GetReaders, Readers}
import io.circe.Json
import io.circe.syntax._
import scorex.core.network.Handshake
import scorex.core.network.peer.PeerManager.ReceivableMessages.GetConnectedPeers
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.{NetworkTime, NetworkTimeProvider}
import scorex.crypto.encode.Base58

import scala.concurrent.Future

case class InfoApiRoute(readersHolder: ActorRef,
                        miner: ActorRef,
                        peerManager: ActorRef,
                        appSettings: EncryAppSettings,
                        nodeId: Array[Byte],
                        timeProvider: NetworkTimeProvider)
                       (implicit val context: ActorRefFactory) extends EncryBaseApiRoute {

  override val settings: RESTApiSettings = appSettings.scorexSettings.restApi

  private val launchTime: NetworkTime.Time = timeProvider.time()

  override val route: Route = (path("info") & get) {
    val nodeUptime = timeProvider.time() - launchTime
    val minerInfoF = getMinerInfo
    val connectedPeersF = getConnectedPeers
    val readersF: Future[Readers] = (readersHolder ? GetReaders).mapTo[Readers]
    (for {
      minerInfo <- minerInfoF
      connectedPeers <- connectedPeersF
      readers <- readersF
    } yield {
      InfoApiRoute.makeInfoJson(nodeId, minerInfo, connectedPeers, readers, getStateType, getNodeName, nodeUptime)
    }).okJson()
  }

  private def getConnectedPeers: Future[Int] = (peerManager ? GetConnectedPeers).mapTo[Seq[Handshake]].map(_.size)

  private def getStateType: String = appSettings.nodeSettings.stateMode.verboseName

  private def getNodeName: String = appSettings.scorexSettings.network.nodeName

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
      "previousFullHeaderId" -> bestFullBlock.map(_.header.parentId).map(Base58.encode).asJson,
      "difficulty" -> bestFullBlock.map(block => DifficultySerializer.decodeCompactBits(block.header.nBits))
        .getOrElse(Constants.Chain.InitialDifficulty).asJson,
      "unconfirmedCount" -> unconfirmedCount.asJson,
      "stateType" -> stateType.asJson,
      "stateVersion" -> stateVersion.asJson,
      "isMining" -> minerInfo.isMining.asJson,
      "peersCount" -> connectedPeersLength.asJson,
      "uptime" -> nodeUptime.asJson
    ).asJson
  }
}
