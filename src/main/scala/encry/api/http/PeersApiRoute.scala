package encry.api.http

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.api.http.PeersApiRoute.PeerInfoResponse
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.RESTApiSettings
import io.circe.Encoder
import encry.EncryApp._
import encry.network.ConnectedPeersList.PeerInfo
import encry.network.PeersKeeper.{GetConnectedPeers, GetInfoAboutConnectedPeers}
import io.circe.generic.semiauto._

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.matching.Regex

case class PeersApiRoute(override val settings: RESTApiSettings)(implicit val context: ActorRefFactory) extends ApiRoute {

  override lazy val route: Route = pathPrefix("peers") { connect }
  private val addressAndPortRegexp: Regex = "\\w+:\\d{1,5}".r

//  def allPeers: Route = (path("all") & get) {
//    val result: Future[immutable.Iterable[PeerInfoResponse]] =
//      (peersKeeper ? GetInfoAboutConnectedPeers).mapTo[Map[InetSocketAddress, PeerInfo]].map {
//        _.map { case (address, peerInfo) =>
//          PeerInfoResponse.fromAddressAndInfo(address, peerInfo)
//        }
//      }
//    onSuccess(result) { r => complete(r) }
//  }
//
  //todo fix this route
//  def connectedPeers: Route = (path("connected") & get) {
//    val result: Future[Seq[PeerInfoResponse]] = (peersKeeper ? GetConnectedPeers).mapTo[Seq[ConnectedPeer]].map {
//      _.map { peer =>
//        PeerInfoResponse(
//          address = peer.socketAddress.toString,
//          name = Some(peer.handshake.nodeName),
//          connectionType = Some(peer.direction.toString))
//      }
//    }
//    onSuccess(result) { r => complete(r) }
//  }

  def connect: Route = (path("connect") & post & entity(as[String])) { body =>
    complete {
      if (addressAndPortRegexp.findFirstMatchIn(body).isDefined) {
        val Array(host, port) = body.split(":")
        val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(host), port.toInt)
        //networkController ! ConnectTo(add)
        StatusCodes.OK
      } else StatusCodes.BadRequest
    }
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

  case class BlacklistedPeers(addresses: Seq[String])

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val encodePeerInfoResponse: Encoder[PeerInfoResponse] = deriveEncoder

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val encodeBlackListedPeers: Encoder[BlacklistedPeers] = deriveEncoder

}