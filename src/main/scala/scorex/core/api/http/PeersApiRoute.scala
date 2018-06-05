package scorex.core.api.http

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import encry.network.Handshake
import io.circe.Encoder
import io.circe.generic.semiauto._
import io.circe.syntax._
import scorex.core.api.http.PeersApiRoute.{BlacklistedPeers, PeerInfoResponse}
import encry.network.peer.PeerInfo
import encry.settings.RESTApiSettings

import scala.concurrent.ExecutionContext.Implicits.global

case class PeersApiRoute(peerManager: ActorRef,
                         networkController: ActorRef,
                         override val settings: RESTApiSettings)(implicit val context: ActorRefFactory)
  extends ApiRoute {

  import encry.network.peer.PeerManager.ReceivableMessages.{GetAllPeers, GetConnectedPeers, GetBlacklistedPeers}
  import encry.network.NetworkController.ReceivableMessages.ConnectTo

  override lazy val route: Route = pathPrefix("peers") { allPeers ~ connectedPeers ~ blacklistedPeers ~ connect }

  def allPeers: Route = (path("all") & get) {
    val result = askActor[Map[InetSocketAddress, PeerInfo]](peerManager, GetAllPeers).map {
      _.map { case (address, peerInfo) =>
        PeerInfoResponse.fromAddressAndInfo(address, peerInfo)
      }
    }
    onSuccess(result) { r => complete(r) }
  }

  def connectedPeers: Route = (path("connected") & get) {
    val now = System.currentTimeMillis()
    val result = askActor[Seq[Handshake]](peerManager, GetConnectedPeers).map {
      _.map { handshake =>
        PeerInfoResponse(
          address = handshake.declaredAddress.map(_.toString).getOrElse(""),
          lastSeen = now,
          name = Some(handshake.nodeName),
          connectionType = None)
      }
    }
    onSuccess(result) { r => complete(r) }
  }

  private val addressAndPortRegexp = "\\w+:\\d{1,5}".r

  //todo: here we receive plain text string, not a json string
  //Quote marks should be successfully parsed here to comply json
  def connect: Route = (path("connect") & post & withAuth & entity(as[String])) { body =>
    complete {
      if (addressAndPortRegexp.findFirstMatchIn(body).isDefined) {
        val Array(host, port) = body.split(":")
        val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(host), port.toInt)
        networkController ! ConnectTo(add)
        StatusCodes.OK
      } else {
        StatusCodes.BadRequest
      }
    }
  }

  def blacklistedPeers: Route = (path("blacklisted") & get) {
    val result = askActor[Seq[String]](peerManager, GetBlacklistedPeers).map { BlacklistedPeers }
    onSuccess(result) { v => complete(v.asJson) }
  }
}

object PeersApiRoute {

  case class PeerInfoResponse(address: String,
                              lastSeen: Long,
                              name: Option[String],
                              connectionType: Option[String])

  object PeerInfoResponse {

    def fromAddressAndInfo(address: InetSocketAddress, peerInfo: PeerInfo): PeerInfoResponse = PeerInfoResponse(
      address.toString,
      peerInfo.lastSeen,
      peerInfo.nodeName,
      peerInfo.connectionType.map(_.toString)
    )
  }

  case class BlacklistedPeers(addresses: Seq[String])

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val encodePeerInfoResponse: Encoder[PeerInfoResponse] = deriveEncoder

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val encodeBlackListedPeers: Encoder[BlacklistedPeers] = deriveEncoder

}

