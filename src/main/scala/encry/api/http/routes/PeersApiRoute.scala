package encry.api.http.routes

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.api.http.DataHolderForApi.{GetAllPeers, GetBannedPeers, GetConnectedPeers}
import encry.api.http.routes.PeersApiRoute.PeerInfoResponse
import encry.cli.commands.AddPeer.PeerFromCli
import encry.network.BlackList.{BanReason, BanTime, BanType}
import encry.network.ConnectedPeersCollection.PeerInfo
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.RESTApiSettings
import io.circe.Encoder
import io.circe.generic.semiauto._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class PeersApiRoute(override val settings: RESTApiSettings, dataHolder: ActorRef, nodeViewSynchronizer: ActorRef)
                        (implicit val context: ActorRefFactory) extends EncryBaseApiRoute {

  override lazy val route: Route = pathPrefix("peers") {
   connectedPeers ~ connectPeer ~ allPeers ~ bannedList
  }

  def allPeers: Route = (path("all") & get) {
    val result: Future[Seq[String]] = (dataHolder ? GetAllPeers)
      .mapTo[Seq[InetSocketAddress]]
      .map(_.map(_.toString))
    onSuccess(result)(r => complete(r))
  }

  def connectedPeers: Route = (path("connected") & get) (
    onSuccess(
      (dataHolder ? GetConnectedPeers)
        .mapTo[Seq[ConnectedPeer]]
        .map(_.map(peer => PeerInfoResponse(
          peer.socketAddress.toString,
          Some(peer.handshake.nodeName),
          Some(peer.direction.toString))
        )))
    (r => complete(r))
  )

  def connectPeer: Route = path("connect") {
    post(entity(as[String]) {
      str =>
        complete {
          Try {
            val split = str.split(':')
            (split(0), split(1).toInt)
          } match {
              case Success((host, port)) =>
                nodeViewSynchronizer ! PeerFromCli(new InetSocketAddress(host, port))
                StatusCodes.OK
              case Failure(_) =>
                StatusCodes.BadRequest
          }
        }
    })
  }

  def bannedList: Route = (path("banned") & get) {
    val result = (dataHolder ? GetBannedPeers)
      .mapTo[Seq[(InetAddress, (BanReason, BanTime, BanType))]]
      .map(_.map(_.toString))
    onSuccess(result)(r => complete(r))
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