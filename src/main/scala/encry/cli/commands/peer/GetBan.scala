package encry.cli.commands.peer

import java.net.InetSocketAddress

import akka.actor.ActorRef
import encry.api.http.DataHolderForApi.{GetConnectedPeersHelper, PeerBanHelper}
import encry.api.http.routes.PeersApiRoute.PeerInfoResponse
import encry.cli.{Ast, Response}
import encry.cli.commands.Command
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import akka.pattern._
import akka.util.Timeout
import encry.network.BlackList.BanReason.InvalidNetworkMessage
import encry.network.PeersKeeper.BanPeerFromAPI

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object GetBan extends Command {

  /**
    * Command "peer ban -host=<addr[String]> -port=<addr[String]>"
    * Example: peer ban -host='172.16.10.57' -port=9020
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       ntp: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    val host: String            = args.requireArg[Ast.Str]("host").s
    val port: Long              = args.requireArg[Ast.Num]("port").i
    val peer: InetSocketAddress = new InetSocketAddress(host, port.toInt)
    dataHolder ! PeerBanHelper(peer, "Banned by the user")
    Future.successful(Some(Response(s"Peer $peer was banned by the user")))
  }
}