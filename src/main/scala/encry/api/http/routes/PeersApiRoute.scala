package encry.api.http.routes

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.api.http.DataHolderForApi._
import encry.api.http.routes.PeersApiRoute.PeerInfoResponse
import encry.network.BlackList.{BanReason, BanTime, BanType}
import encry.network.ConnectedPeersCollection.PeerInfo
import encry.settings.RESTApiSettings
import io.circe.Encoder
import io.circe.generic.semiauto._
import io.circe.syntax._
import scala.util.{Failure, Success, Try}

case class PeersApiRoute(override val settings: RESTApiSettings, dataHolder: ActorRef)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute {

  override lazy val route: Route = pathPrefix("peers") {
    connectedPeers ~ allPeers ~ bannedList ~ WebRoute.authRoute(connectPeer ~ removeFromBan, settings)
  }

  def allPeers: Route = (path("all") & get) {
    (dataHolder ? GetAllPeers)
      .mapTo[Seq[InetSocketAddress]]
      .map(_.map(_.toString).asJson).okJson()
  }

  def connectedPeers: Route = (path("connected") & get) {
    (dataHolder ? GetConnectedPeersHelper)
      .mapTo[Seq[PeerInfoResponse]].map(_.asJson).okJson()
  }

  def bannedList: Route = (path("banned") & get) {
    (dataHolder ? GetBannedPeersHelper).mapTo[Seq[(InetAddress, (BanReason, BanTime, BanType))]]
    .map(_.map(_.toString).asJson).okJson()
  }

  def connectPeer: Route = path("add") {
    post(entity(as[String]) { str =>
      complete {
        Try {
          val split = str.split(':')
          (split(0), split(1).toInt)
        } match {
          case Success((host, port)) =>
            dataHolder ! UserAddPeer(new InetSocketAddress(host, port))
            StatusCodes.OK
          case Failure(_) =>
            StatusCodes.BadRequest
        }
      }
    })
  }

  def removeFromBan: Route = path("remove") {
    post(entity(as[String]) { str =>
      complete {
        Try {
          val split = str.split(':')
          (split(0), split(1).toInt)
        } match {
          case Success((host, port)) =>
            dataHolder ! RemovePeerFromBanList(new InetSocketAddress(host, port))
            StatusCodes.OK
          case Failure(_) =>
            StatusCodes.BadRequest
        }
      }
    })
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