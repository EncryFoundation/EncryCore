package encry.cli.commands

import akka.actor.ActorRef
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.api.http.DataHolderForApi.ShutdownNode
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object NodeShutdown extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings, dataHolder: ActorRef): Future[Option[Response]] = {
    dataHolder ! ShutdownNode
    Future(None)
  }
}
