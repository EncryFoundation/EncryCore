package encry.cli.commands

import java.net.InetSocketAddress
import encry.EncryApp._
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings
import scala.concurrent.Future

/**
  * Command "settings addPeer -host=<addr[String]> -port=<addr[String]>"
  * Example: settings addPeer -host='172.16.11.12' -port=9040
  */
object AddPeer extends Command {
  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    val host: String = args.requireArg[Ast.Str]("host").s
    val port: Long = args.requireArg[Ast.Num]("port").i
    val peer: InetSocketAddress = new InetSocketAddress(host, port.toInt)
    nodeViewSynchronizer ! PeerFromCli(peer)
    Future(Some(Response("Peer added!")))
  }

  case class PeerFromCli(address: InetSocketAddress)
}