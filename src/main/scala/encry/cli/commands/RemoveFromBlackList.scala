package encry.cli.commands

import java.net.{InetAddress, InetSocketAddress}
import encry.EncryApp._
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings
import scala.concurrent.Future

/**
  * Command "settings removeFromBlackList -host=<addr[String]> -port=<addr[String]>"
  * Example: settings removeFromBlackList -host='10.101.0.30' -port=53648
  */

object RemoveFromBlackList extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    val host: String = args.requireArg[Ast.Str]("host").s
    val port: Long = args.requireArg[Ast.Num]("port").i
    val peer: InetSocketAddress = new InetSocketAddress(host, port.toInt)
    nodeViewSynchronizer ! RemovePeerFromBlackList(peer)
    Future(Some(Response("Peer removed from black list")))
  }

  final case class RemovePeerFromBlackList(address: InetSocketAddress)
}