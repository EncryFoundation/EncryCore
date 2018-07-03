package encry.cli.commands

import akka.util.Timeout
import encry.EncryApp.nodeViewHolder
import encry.cli.Response
import encry.settings.EncryAppSettings

import scala.concurrent.Future
import akka.pattern._

trait ViewCommand extends Command {
  override def executeRequest(args: Command.Args, settings: EncryAppSettings): Any = LocalCommand
  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nodeViewHolder ? executeRequest(args, settings)).mapTo[Option[Response]]
  }
}
