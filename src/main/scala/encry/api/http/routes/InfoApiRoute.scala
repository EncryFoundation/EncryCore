package encry.api.http.routes

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.local.miner.Miner.{GetMinerStatus, MinerStatus}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.GetConnectedPeers
import encry.settings._
import encry.EncryApp._
import encry.utils.{NetworkTime, NetworkTimeProvider}
import encry.view.ReadersHolder.{GetReaders, Readers}
import encry.view.mempool.Mempool.GetMempoolSize
import io.circe.Json
import io.circe.syntax._
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.constants.TestNetConstants

import scala.concurrent.Future

case class InfoApiRoute(readersHolder: ActorRef,
                        miner: ActorRef,
                        appSettings: EncryAppSettings,
                        nodeId: Array[Byte],
                        mempool: ActorRef,
                        timeProvider: NetworkTimeProvider)
                       (implicit val context: ActorRefFactory) extends EncryBaseApiRoute {

  override val settings: RESTApiSettings = appSettings.restApi

  private val launchTimeFuture: Future[NetworkTime.Time] = timeProvider.time()

  override val route: Route = (path("info") & get) {
    val minerInfoF: Future[MinerStatus] = getMinerInfo
//    val connectedPeersF: Future[Int] = getConnectedPeers
    val readersF: Future[Readers] = (readersHolder ? GetReaders).mapTo[Readers]
    val mempoolSize: Future[Int] = (mempool ? GetMempoolSize).mapTo[Int]
    (for {
      minerInfo <- minerInfoF
//      connectedPeers <- connectedPeersF
      readers <- readersF
      currentTime <- timeProvider.time()
      launchTime <- launchTimeFuture
      memSize <- mempoolSize
      storage = storageInfo
      nodeUptime = currentTime - launchTime
    } yield InfoApiRoute.makeInfoJson(nodeId, minerInfo,
      1,
      readers, getStateType, getNodeName,
      getAddress, storage, nodeUptime, memSize, getConnectionWithPeers)
      ).okJson()
  }

//  private def getConnectedPeers: Future[Int] = (peersKeeper ? GetConnectedPeers).mapTo[Seq[ConnectedPeer]].map(_.size)

  private def getStateType: String = appSettings.node.stateMode.verboseName

  private def getNodeName: String = appSettings.network.nodeName
    .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + appSettings.network.bindAddress.getPort)

  private def storageInfo: String = if (appSettings.postgres.exists(_.enableSave)) "Postgres(write)" else ""

  //private def restoreInfo: Future[Boolean] = (peersKeeper ? GetRecoveryStatus).mapTo[Boolean]

  private def getAddress: Seq[InetSocketAddress] = appSettings.network.knownPeers

  private def getMinerInfo: Future[MinerStatus] = (miner ? GetMinerStatus).mapTo[MinerStatus]

  private def getConnectionWithPeers: Boolean = appSettings.network.connectOnlyWithKnownPeers.getOrElse(false)
}

object InfoApiRoute {

  def makeInfoJson(nodeId: Array[Byte],
                   minerInfo: MinerStatus,
                   connectedPeersLength: Int,
                   readers: Readers,
                   stateType: String,
                   nodeName: String,
                   knownPeers: Seq[InetSocketAddress],
                   storage: String,
                   nodeUptime: Long,
                   mempoolSize: Int,
                   connectWithOnlyKnownPeer: Boolean): Json = {
    val stateVersion: Option[String] = readers.s.map(_.version).map(Algos.encode)
    val bestHeader: Option[Header] = readers.h.flatMap(_.bestHeaderOpt)
    val bestFullBlock: Option[Block] = readers.h.flatMap(_.bestBlockOpt)
    //val unconfirmedCount: Int = readers.m.map(_.size).getOrElse(0)
    Map(
      "name" -> nodeName.asJson,
      "headersHeight" -> bestHeader.map(_.height).getOrElse(0).asJson,
      "fullHeight" -> bestFullBlock.map(_.header.height).getOrElse(0).asJson,
      "bestHeaderId" -> bestHeader.map(_.encodedId).asJson,
      "bestFullHeaderId" -> bestFullBlock.map(_.header.encodedId).asJson,
      "previousFullHeaderId" -> bestFullBlock.map(_.header.parentId).map(Algos.encode).asJson,
      "difficulty" -> bestFullBlock.map(block => block.header.difficulty.toString)
        .getOrElse(TestNetConstants.InitialDifficulty.toString).asJson,
      "unconfirmedCount" -> mempoolSize.asJson,
      "stateType" -> stateType.asJson,
      "stateVersion" -> stateVersion.asJson,
      "isMining" -> minerInfo.isMining.asJson,
      "peersCount" -> connectedPeersLength.asJson,
      "knownPeers" -> knownPeers.map { x => x.getHostName + ":" + x.getPort }.asJson,
      "storage" -> storage.asJson,
      "uptime" -> nodeUptime.asJson,
      "isConnectedWithKnownPeers" -> connectWithOnlyKnownPeer.asJson,
    ).asJson
  }
}