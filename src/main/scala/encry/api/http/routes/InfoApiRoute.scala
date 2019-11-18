package encry.api.http.routes

import java.net.InetSocketAddress
import akka.actor.{ ActorRef, ActorRefFactory }
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.api.http.DataHolderForApi._
import encry.local.miner.Miner.MinerStatus
import encry.settings._
import encry.utils.NetworkTimeProvider
import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import org.encryfoundation.common.modifiers.history.{ Block, Header }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.constants.Constants

case class InfoApiRoute(dataHolder: ActorRef,
                        settings: RESTApiSettings,
                        nodeId: Array[Byte],
                        timeProvider: NetworkTimeProvider)(implicit val context: ActorRefFactory)
    extends EncryBaseApiRoute {

  override val route: Route = (path("info") & get) {
    (dataHolder ? GetAllInfoHelper)
      .mapTo[Json]
      .okJson()
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
    val stateRoot: Option[String] = readers.s.map(_.tree.rootHash).map(Algos.encode)
    val prevFullHeaderId: String = block.map(b => Algos.encode(b.header.parentId)).getOrElse("")
    InfoApi(
      nodeName,
      stateType,
      block.map(_.header.difficulty.toString).getOrElse(constants.InitialDifficulty.toString),
      block.map(_.encodedId).getOrElse(""),
      header.map(_.encodedId).getOrElse(""),
      connectedPeersLength,
      mempoolSize,
      prevFullHeaderId,
      block.map(_.header.height).getOrElse(0),
      header.map(_.height).getOrElse(0),
      stateVersion.getOrElse(""),
      nodeUptime,
      storage,
      connectWithOnlyKnownPeer,
      minerInfo.isMining,
      knownPeers.map { x =>
        x.getHostName + ":" + x.getPort
      },
      stateRoot.getOrElse("")
    ).asJson
  }
}