package encry.cli.commands

import encry.EncryApp._
import encry.api.http.DataHolderForApi.{StopMiner}
import encry.cli.Response
import encry.settings.EncryAppSettings

import scala.concurrent.Future

object StopMine extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    dataHolderForApi ! StopMiner
    Future(Some(Response("Mining is stopped.")))
  }
}
