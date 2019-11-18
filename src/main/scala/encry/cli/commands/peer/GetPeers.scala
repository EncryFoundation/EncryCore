package encry.cli.commands.peer

import java.net.InetSocketAddress
import akka.actor.ActorRef
import encry.cli.Response
import encry.cli.commands.Command
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import akka.pattern._
import akka.util.Timeout
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import encry.api.http.DataHolderForApi.GetAllPeers

object GetPeers extends Command {

  /**
    * Command "peer all"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       ntp: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ? GetAllPeers)
      .mapTo[Seq[InetSocketAddress]]
      .map(x => Some(Response(x.toString())))
  }
}
