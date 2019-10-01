package encry.cli.commands

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorRef
import encry.api.http.DataHolderForApi.StopMiner
import encry.cli.Response
import encry.settings.EncryAppSettings
import scala.concurrent.Future

object StopMine extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings, dataHolder: ActorRef): Future[Option[Response]] = {
    dataHolder ! StopMiner
    Future(Some(Response("Mining is stopped.")))
  }
}
