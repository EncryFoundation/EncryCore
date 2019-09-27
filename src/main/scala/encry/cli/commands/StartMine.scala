package encry.cli.commands

import encry.EncryApp._
import encry.api.http.DataHolderForApi.{StartMiner}
import encry.cli.Response
import encry.settings.EncryAppSettings

import scala.concurrent.Future

object StartMine extends Command {


  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    dataHolderForApi ! StartMiner
    Future(Some(Response("Mining is started.")))
  }
}
