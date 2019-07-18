package encry.api.http.routes

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.local.miner.Miner.MinerStatus
import encry.settings._
import encry.api.http.DataHolderForApi.{BlockInfo, GetBlockInfo, GetConnectedPeers, GetMinerStatus, GetReaders, GetTransactionsNumber, Readers}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.utils.{NetworkTime, NetworkTimeProvider}
import io.circe.Json
import io.circe.syntax._
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.constants.TestNetConstants
import scala.concurrent.Future

case class InfoApiRoute(dataHolder: ActorRef,
                        appSettings: EncryAppSettings,
                        nodeId: Array[Byte],
                        timeProvider: NetworkTimeProvider)
                       (implicit val context: ActorRefFactory) extends EncryBaseApiRoute {

  override val settings: RESTApiSettings = appSettings.restApi

  private val getNodeName: String = appSettings.network.nodeName
    .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + appSettings.network.bindAddress.getPort)

  private val getStateType: String = "UTXO"

  private val storageInfo: String = if (appSettings.postgres.exists(_.enableSave)) "Postgres(write)" else ""

  private val getAddress: Seq[InetSocketAddress] = appSettings.network.knownPeers

  private val getConnectionWithPeers: Boolean = appSettings.network.connectOnlyWithKnownPeers.getOrElse(false)

  private val launchTimeFuture: Future[NetworkTime.Time] = timeProvider.time()

  override val route: Route = (path("info") & get) {
    val minerInfoF: Future[MinerStatus] = getMinerInfo
    val connectedPeersF: Future[Int] = getConnectedPeers
    val readersF: Future[Readers] = (dataHolder ? GetReaders).mapTo[Readers]
    val mempoolSize: Future[Int] = (dataHolder ? GetTransactionsNumber).mapTo[Int]
    val blockInfoF: Future[BlockInfo] = (dataHolder ? GetBlockInfo).mapTo[BlockInfo]
    (for {
      minerInfo         <- minerInfoF
      connectedPeers    <- connectedPeersF
      readers           <- readersF
      currentTime       <- timeProvider.time()
      launchTime        <- launchTimeFuture
      memSize           <- mempoolSize
      nodeUptime        = currentTime - launchTime
      bInfo             <- blockInfoF
      headerHeight      = bInfo.headerHeight
      headerId          = bInfo.headerId
      fullHeight        = bInfo.fullHeight
      bestFullHeaderId  = bInfo.bestFullHeaderId
    } yield InfoApiRoute.makeInfoJson(
      nodeId,
      minerInfo,
      connectedPeers,
      readers,
      getStateType,
      getNodeName,
      getAddress,
      storageInfo,
      nodeUptime,
      memSize,
      getConnectionWithPeers,
      headerHeight,
      headerId,
      fullHeight,
      bestFullHeaderId)).okJson()
  }

  private def getConnectedPeers: Future[Int] = (dataHolder ? GetConnectedPeers).mapTo[Seq[ConnectedPeer]].map(_.size)

  private def getMinerInfo: Future[MinerStatus] = (dataHolder ? GetMinerStatus).mapTo[MinerStatus]
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
                   connectWithOnlyKnownPeer: Boolean,
                   headerHeight: Int,
                   headerId: String,
                   fullHeight: Int,
                   bestFullHeaderId: String
                  ): Json = {
    val stateVersion: Option[String] = readers.s.map(_.version).map(Algos.encode)
    val bestFullBlock: Option[Block] = readers.h.flatMap(_.bestBlockOpt)
    val prevFullHeaderId: String = bestFullBlock.map(b => Algos.encode(b.header.parentId)).getOrElse("")
    Map(
      "name"                      -> nodeName.asJson,
      "headersHeight"             -> headerHeight.asJson,
      "fullHeight"                -> fullHeight.asJson,
      "bestHeaderId"              -> headerId.asJson,
      "bestFullHeaderId"          -> bestFullHeaderId.asJson,
      "previousFullHeaderId"      -> prevFullHeaderId.asJson,
      "difficulty"                -> bestFullBlock.map(block => block.header.difficulty.toString)
        .getOrElse(TestNetConstants.InitialDifficulty.toString).asJson,
      "unconfirmedCount"          -> mempoolSize.asJson,
      "stateType"                 -> stateType.asJson,
      "stateVersion"              -> stateVersion.asJson,
      "isMining"                  -> minerInfo.isMining.asJson,
      "peersCount"                -> connectedPeersLength.asJson,
      "knownPeers"                -> knownPeers.map { x => x.getHostName + ":" + x.getPort }.asJson,
      "storage"                   -> storage.asJson,
      "uptime"                    -> nodeUptime.asJson,
      "isConnectedWithKnownPeers" -> connectWithOnlyKnownPeer.asJson,
    ).asJson
  }
}