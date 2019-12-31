package encry.cli.commands

import java.net.InetSocketAddress

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorRef
import encry.api.http.DataHolderForApi.RemovePeerFromBanList
import encry.cli.{ Ast, Response }
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider

import scala.concurrent.Future

/**
 * Command "peer removeFromBlackList -host=<addr[String]> -port=<addr[String]>"
 * Example: peer removeFromBlackList -host='10.101.0.30' -port=53648
 */
object RemoveFromBlackList extends Command {

  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    val host: String            = args.requireArg[Ast.Str]("host").s
    val port: Long              = args.requireArg[Ast.Num]("port").i
    val peer: InetSocketAddress = new InetSocketAddress(host, port.toInt)
    dataHolder ! RemovePeerFromBanList(peer)
    Future.successful(Some(Response(s"Peer $peer was removed from black list")))
  }
}
