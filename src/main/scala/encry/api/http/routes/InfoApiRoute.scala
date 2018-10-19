package encry.api.http.routes

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.local.miner.Miner.{GetMinerStatus, MinerStatus}
import encry.modifiers.history.Block
import encry.modifiers.history.Header
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeerManager.ReceivableMessages.{GetConnectedPeers, GetRecoveryStatus}
import encry.settings._
import encry.utils.{NetworkTime, NetworkTimeProvider}
import encry.view.ReadersHolder.{GetReaders, Readers}
import io.circe.Json
import io.circe.syntax._
import org.encryfoundation.common.Algos
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
      storage <- storageInfo
      nodeUptime = currentTime - launchTime
    } yield InfoApiRoute.makeInfoJson(nodeId, minerInfo, connectedPeers, readers, getStateType, getNodeName,
      getAddress, storage, nodeUptime, getConnectionWithPeers)
      ).okJson()
  }

  private def getConnectedPeers: Future[Int] = (peerManager ? GetConnectedPeers).mapTo[Seq[ConnectedPeer]].map(_.size)

  private def getStateType: String = appSettings.node.stateMode.verboseName

  private def getNodeName: String = appSettings.network.nodeName
    .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + appSettings.network.bindAddress.getPort)

  private def storageInfo: Future[String] = for {
    recoveryStatus <- restoreInfo
  } yield {
    (appSettings.levelDb, appSettings.postgres) match {
      case (Some(LevelDbSettings(levelDbSave, levelDbRestore, _)),
      Some(PostgresSettings(_, _, _, _, pgSave, pgRestore, _, _))) =>
        if ((levelDbSave || (levelDbRestore && (!recoveryStatus))) && (pgSave || (pgRestore && (!recoveryStatus))))
          s"LevelDb(${if (levelDbSave) "write" else ""} ${if (levelDbRestore && (!recoveryStatus)) "read" else ""}), " +
            s"Postgres(${if (pgSave) "write" else ""} ${if (pgRestore && (!recoveryStatus)) "read" else ""})"
        else if (levelDbSave || (levelDbRestore && (!recoveryStatus)))
          s"LevelDb(${if (levelDbSave) "write" else ""} ${if (levelDbRestore && (!recoveryStatus)) "read" else ""})"
        else if (pgSave || (pgRestore && (!recoveryStatus)))
          s"Postgres(${if (pgSave) "write" else ""} ${if (pgRestore && (!recoveryStatus)) "read" else ""})"
        else ""
      case (Some(LevelDbSettings(save, restore, _)), None) if save || restore =>
        if (save && (restore && !recoveryStatus)) "LevelDb(read, write)"
        else if (save && !restore) "LevelDb(write)"
        else if (!save && (restore && !recoveryStatus)) "LevelDb(read)"
        else ""
      case (None, Some(PostgresSettings(_, _, _, _, save, restore, _, _))) if save || restore =>
        if (save && (restore && !recoveryStatus)) "Postgres(read, write)"
        else if (save && !restore) "Postgres(write)"
        else if (!save && (restore && !recoveryStatus)) "Postgres(read)"
        else ""
      case _ => ""
    }
  }

  private def restoreInfo: Future[Boolean] = (peerManager ? GetRecoveryStatus).mapTo[Boolean]

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
                   connectWithOnlyKnownPeer: Boolean): Json = {
    val stateVersion: Option[String] = readers.s.map(_.version).map(Algos.encode)
    val bestHeader: Option[Header] = readers.h.flatMap(_.bestHeaderOpt)
    val bestFullBlock: Option[Block] = readers.h.flatMap(_.bestBlockOpt)
    val unconfirmedCount: Int = readers.m.map(_.size).getOrElse(0)
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
      "knownPeers" -> knownPeers.map { x => x.getHostName + ":" + x.getPort }.asJson,
      "storage" -> storage.asJson,
      "uptime" -> nodeUptime.asJson,
      "isConnectedWithKnownPeers" -> connectWithOnlyKnownPeer.asJson,
    ).asJson
  }
}