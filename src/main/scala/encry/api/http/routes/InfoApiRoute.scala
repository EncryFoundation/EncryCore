package encry.api.http.routes

import java.net.InetSocketAddress
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.api.http.DataHolderForApi._
import encry.local.miner.Miner.MinerStatus
import encry.settings._
import encry.utils.NetworkTimeProvider
import io.circe.Json
import io.circe.syntax._
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.constants.Constants

case class InfoApiRoute(dataHolder: ActorRef,
                        appSettings: EncryAppSettings,
                        nodeId: Array[Byte],
                        timeProvider: NetworkTimeProvider)
                       (implicit val context: ActorRefFactory) extends EncryBaseApiRoute {

  override val settings: RESTApiSettings = appSettings.restApi

  override val route: Route = (path("info") & get) {
    (dataHolder ? GetAllInfoHelper)
      .mapTo[Json].okJson()
  }
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
                   header: Option[Header],
                   block: Option[Block],
                   constants: Constants
                  ): Json = {
    val stateVersion: Option[String] = readers.s.map(_.version).map(Algos.encode)
    val prevFullHeaderId: String = block.map(b => Algos.encode(b.header.parentId)).getOrElse("")
    Map(
      "name"                      -> nodeName.asJson,
      "headersHeight"             -> header.map(_.height).getOrElse(0).asJson,
      "fullHeight"                -> block.map(_.header.height).getOrElse(0).asJson,
      "bestHeaderId"              -> header.map(_.encodedId).getOrElse("").asJson,
      "bestFullHeaderId"          -> block.map(_.encodedId).getOrElse("").asJson,
      "previousFullHeaderId"      -> prevFullHeaderId.asJson,
      "difficulty"                -> block.map(_.header.difficulty.toString)
        .getOrElse(constants.InitialDifficulty.toString).asJson,
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