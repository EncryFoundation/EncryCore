package encry.cli.commands.info

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorRef
import encry.api.http.DataHolderForApi.GetAllInfoHelper
import encry.cli.Response
import encry.cli.commands.Command
import encry.settings.EncryAppSettings
import akka.pattern._
import akka.util.Timeout
import encry.utils.NetworkTimeProvider
import io.circe._
import scala.concurrent._

object GetInfo extends Command {

  /**
    * Command "app info"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ? GetAllInfoHelper)
      .mapTo[Json]
      .map(x => Some(Response(x.toString())))
  }
}
