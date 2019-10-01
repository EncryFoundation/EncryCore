package encry.cli.commands

import akka.actor.ActorRef
import scala.concurrent.ExecutionContext.Implicits.global
import encry.api.http.DataHolderForApi.StartMiner
import encry.cli.Response
import encry.settings.EncryAppSettings
import scala.concurrent.Future

object StartMine extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings, dataHolder: ActorRef): Future[Option[Response]] = {
    dataHolder ! StartMiner
    Future(Some(Response("Mining is started.")))
  }
}
