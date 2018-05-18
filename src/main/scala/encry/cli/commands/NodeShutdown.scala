package encry.cli.commands

import encry.EncryApp
import encry.cli.Response
import encry.settings.EncryAppSettings
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object NodeShutdown extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    EncryApp.forceStopApplication()
    Future(None)
  }
}
