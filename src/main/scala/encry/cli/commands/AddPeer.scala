package encry.cli.commands

import java.net.InetSocketAddress
import akka.actor.ActorRef
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Command "settings addPeer -host=<addr[String]> -port=<addr[String]>"
  * Example: settings addPeer -host='10.101.0.30' -port=53648
  */
object AddPeer extends Command {
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       ncRef: ActorRef,
                       nvhRef: ActorRef,
                       minerRef: ActorRef,
                       nvshRef: ActorRef,
                       mempoolRef: ActorRef): Future[Option[Response]] = {
    val host: String = args.requireArg[Ast.Str]("host").s
    val port: Long = args.requireArg[Ast.Num]("port").i
    val peer: InetSocketAddress = new InetSocketAddress(host, port.toInt)
    ncRef ! PeerFromCli(peer)
    Future(Some(Response("Peer added!")))
  }

  case class PeerFromCli(address: InetSocketAddress)

}