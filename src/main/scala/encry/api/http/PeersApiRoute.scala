package encry.api.http

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.api.http.PeersApiRoute.PeerInfoResponse
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.NetworkController.ReceivableMessages.ConnectTo
import encry.network.PeerInfo
import encry.network.PeerManager.ReceivableMessages.{GetAllPeers, GetConnectedPeers}
import encry.settings.RESTApiSettings
import io.circe.generic.auto._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.media.{ArraySchema, Content, ExampleObject, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.responses.ApiResponse
import javax.ws.rs.{GET, POST, Path}
import scala.annotation.meta.field
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.matching.Regex

@Path("/peers")
case class PeersApiRoute(peerManager: ActorRef,
                         networkController: ActorRef,
                         override val settings: RESTApiSettings)(implicit val context: ActorRefFactory) extends ApiRoute {

  override lazy val route: Route = pathPrefix("peers") { allPeers ~ connectedPeers ~ connect }
  private val addressAndPortRegexp: Regex = "\\w+:\\d{1,5}".r

  @GET
  @Path("/all")
  @Operation(summary = "Return all known peers",
    responses = Array(
    new ApiResponse(responseCode = "200", description = "Array of peers",
      content = Array(new Content(mediaType = "application/json",
        array = new ArraySchema(schema = new Schema(implementation = classOf[PeerInfoResponse]))))),
    new ApiResponse(responseCode = "500", description = "Internal server error")),
  )
  def allPeers: Route = (path("all") & get) {
    val result: Future[immutable.Iterable[PeerInfoResponse]] =
      (peerManager ? GetAllPeers).mapTo[Map[InetSocketAddress, PeerInfo]].map {
      _.map { case (address, peerInfo) =>
        PeerInfoResponse.fromAddressAndInfo(address, peerInfo)
      }
    }
    onSuccess(result) { r => complete(r) }
  }

  @GET
  @Path("/connected")
  @Operation(summary = "Return all connected peers",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Array of connected peers",
        content = Array(new Content(mediaType = "application/json",
          array = new ArraySchema(schema = new Schema(implementation = classOf[PeerInfoResponse]))))),
      new ApiResponse(responseCode = "500", description = "Internal server error"))
  )
  def connectedPeers: Route = (path("connected") & get) {
    val result: Future[Seq[PeerInfoResponse]] = (peerManager ? GetConnectedPeers).mapTo[Seq[ConnectedPeer]].map {
      _.map { peer =>
        PeerInfoResponse(
          address = peer.socketAddress.toString,
          name = Some(peer.handshake.nodeName),
          connectionType = Some(peer.direction.toString))
      }
    }
    onSuccess(result) { r => complete(r) }
  }

  @POST
  @Path("/connect")
  @Operation(summary = "Add address to peers list",
    requestBody = new RequestBody(content =
      Array(new Content(schema =
        new Schema(implementation = classOf[String]), examples = Array(new ExampleObject(value = "127.0.0.1:5673")))
      ),
      required = true
    ),
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Attempt to connect to the peer"),
      new ApiResponse(responseCode = "400", description = "Bad request"),
      new ApiResponse(responseCode = "500", description = "Internal server error"))
  )
  def connect: Route = (path("connect") & post & entity(as[String])) { body =>
    complete {
      if (addressAndPortRegexp.findFirstMatchIn(body).isDefined) {
        val Array(host, port) = body.split(":")
        val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(host), port.toInt)
        networkController ! ConnectTo(add)
        StatusCodes.OK
      } else StatusCodes.BadRequest
    }
  }
}

object PeersApiRoute {

  @ApiModel(description = "Connected peer")
  case class PeerInfoResponse(
    @(ApiModelProperty @field)(value = "ip address and port of a  peer", example = "127.0.0.1:5673") address: String,
    @(ApiModelProperty @field)(value = "name of a peer node", required = false, example = "mynode") name: Option[String],
    @(ApiModelProperty @field)(value = "incoming or outgoing connection", required = false) connectionType: Option[String]
  )

  object PeerInfoResponse {

    def fromAddressAndInfo(address: InetSocketAddress, peerInfo: PeerInfo): PeerInfoResponse = PeerInfoResponse(
      address.toString,
      peerInfo.nodeName,
      peerInfo.connectionType.map(_.toString)
    )
  }
}