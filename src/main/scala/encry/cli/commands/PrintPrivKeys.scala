package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.api.http.DataHolderForApi.GetViewPrintPrivKeys
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

//TODO This cmd is unsafe.
object PrintPrivKeys extends Command {

  /**
    * Command "wallet privKeys"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ? GetViewPrintPrivKeys).mapTo[String].map(s => Some(Response(s)))
  }
}
