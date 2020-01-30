package encry.cli.commands

import java.net.InetSocketAddress

import encry.api.http.DataHolderForApi.UserAddPeer
import akka.actor.ActorRef

import scala.concurrent.ExecutionContext.Implicits.global
import encry.cli.{ Ast, Response }
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider

import scala.concurrent.Future

/**
 * Command "peer addPeer -host=<addr[String]> -port=<addr[String]>"
 * Example: peer addPeer -host='172.16.11.28' -port=9040
 * Example: peer addPeer -host='192.168.1.122' -port=9001
 */
object AddPeer extends Command {
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    val host: String            = args.requireArg[Ast.Str]("host").s
    val port: Long              = args.requireArg[Ast.Num]("port").i
    val peer: InetSocketAddress = new InetSocketAddress(host, port.toInt)
    dataHolder ! UserAddPeer(peer)
    Future.successful(Some(Response("Peer added!")))
  }
}
