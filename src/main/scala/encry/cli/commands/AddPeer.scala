package encry.cli.commands

import java.net.InetSocketAddress

import encry.api.http.DataHolderForApi.PeerAdd
import akka.actor.ActorRef

import scala.concurrent.ExecutionContext.Implicits.global
import encry.cli.{ Ast, Response }
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider

import scala.concurrent.Future

/**
 * Command "settings addPeer -host=<addr[String]> -port=<addr[String]>"
 * Example: settings addPeer -host='172.16.10.57' -port=9020
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
    dataHolder ! PeerAdd(peer)
    Future(Some(Response("Peer added!")))
  }
}
