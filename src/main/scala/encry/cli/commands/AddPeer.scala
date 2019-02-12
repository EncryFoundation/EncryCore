package encry.cli.commands

import java.net.InetSocketAddress
import encry.EncryApp.system
import scala.concurrent.ExecutionContext.Implicits.global
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings
import scala.concurrent.Future

/**
  * Command "app peer add -host=<addr[String]> -port=<addr[String]>"
  * app peer -host='0.0.0.0' -port=0000
  */
object AddPeer extends Command {
  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    val host: String = args.requireArg[Ast.Str]("host").s
    val port: Long = args.requireArg[Ast.Num]("port").i
    val peer: InetSocketAddress = new InetSocketAddress(host, port.toInt)
    system.actorSelection("/user/peerManager") ! PeerFromCli(peer)
    Future(Some(Response("Peer added!")))
  }

  case class PeerFromCli(address: InetSocketAddress)
}