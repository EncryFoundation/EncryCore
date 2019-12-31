package encry.cli.commands.peer

import akka.actor.ActorRef
import encry.api.http.DataHolderForApi.GetConnectedPeersHelper
import encry.api.http.routes.PeersApiRoute.PeerInfoResponse
import encry.cli.Response
import encry.cli.commands.Command
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import akka.pattern._
import akka.util.Timeout
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object GetConnectedPeers extends Command {

  /**
    * Command "peer connected"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       ntp: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ? GetConnectedPeersHelper).mapTo[Seq[PeerInfoResponse]].map(x => Some(Response(x.toString())))
  }
}
