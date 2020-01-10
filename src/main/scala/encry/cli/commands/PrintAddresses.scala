package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.api.http.DataHolderForApi.GetViewPrintAddress
import encry.utils.NetworkTimeProvider
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object PrintAddresses extends Command {

  /**
    * Command "wallet addrs"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ? GetViewPrintAddress).mapTo[String].map(s => Some(Response(s)))
  }
}
