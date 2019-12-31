package encry.cli.commands.peer

import java.net.InetAddress
import akka.actor.ActorRef
import encry.cli.Response
import encry.cli.commands.Command
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import akka.pattern._
import akka.util.Timeout
import encry.api.http.DataHolderForApi.GetBannedPeersHelper
import encry.network.BlackList.{BanReason, BanTime, BanType}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object GetBannedPeers extends Command {

  /**
    * Command "peer banned"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       ntp: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ? GetBannedPeersHelper).mapTo[Seq[(InetAddress, (BanReason, BanTime, BanType))]]
      .map(x => Some(Response(x.toString)))
  }
}
