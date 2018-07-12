package encry.cli

import akka.pattern._
import akka.util.Timeout
import encry.EncryApp.nodeViewHolder
import encry.settings.EncryAppSettings

import scala.concurrent.Future

trait ViewCommand extends Command {
  override def executeRequest(args: Command.Args, settings: EncryAppSettings): Any = this
  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nodeViewHolder ? executeRequest(args, settings)).mapTo[Option[Response]]
  }
}
