package encry.api.http.routes

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.api.http.DataHolderForApi.{GetAllPeers, GetBannedPeers, GetConnectedPeers}
import encry.api.http.routes.PeersApiRoute.PeerInfoResponse
import encry.network.BlackList.{BanReason, BanTime, BanType}
import encry.network.ConnectedPeersCollection.PeerInfo
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.RESTApiSettings
import io.circe.Encoder
import io.circe.generic.semiauto._
import scala.concurrent.Future

case class PeersApiRoute(override val settings: RESTApiSettings,
                         dataHolder: ActorRef)(implicit val context: ActorRefFactory) extends EncryBaseApiRoute {

  override lazy val route: Route = pathPrefix("peers") {
   connectedPeers ~ allPeers ~ bannedList
  }

  def allPeers: Route = (path("all") & get) {
    val result: Future[Seq[String]] = (dataHolder ? GetAllPeers)
      .mapTo[Seq[InetSocketAddress]]
      .map(_.map(_.toString))
    onSuccess(result)(r => complete(r))
  }

  def connectedPeers: Route = (path("connected") & get) {
    val result: Future[Seq[PeerInfoResponse]] = (dataHolder ? GetConnectedPeers).mapTo[Seq[ConnectedPeer]].map {
      _.map { peer =>
        PeerInfoResponse(
          address = peer.socketAddress.toString,
          name = Some(peer.handshake.nodeName),
          connectionType = Some(peer.direction.toString))
      }
    }
    onSuccess(result) { r => complete(r) }
  }

  def bannedList: Route = (path("banned") & get) {
    val result = (dataHolder ? GetBannedPeers).mapTo[Seq[(InetAddress, (BanReason, BanTime, BanType))]].map {
      _.map(_.toString)
      }
    onSuccess(result) {r => complete(r)}
  }
}

object PeersApiRoute {

  case class PeerInfoResponse(address: String, name: Option[String], connectionType: Option[String])

  object PeerInfoResponse {

    def fromAddressAndInfo(address: InetSocketAddress, peerInfo: PeerInfo): PeerInfoResponse = PeerInfoResponse(
      address.toString,
      Some(peerInfo.connectedPeer.toString),
      Some(peerInfo.connectionType.toString)
    )
  }

  implicit val encodePeerInfoResponse: Encoder[PeerInfoResponse] = deriveEncoder
}