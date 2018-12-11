package encry.cli.commands

import encry.EncryApp._
import encry.cli.Response
import encry.local.miner.Miner.EnableMining
import encry.settings.EncryAppSettings
import scala.concurrent.Future

object StartMine extends Command {

  import encry.local.miner.Miner.StartMining

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    miner ! EnableMining
    miner ! StartMining
    Future(Some(Response("Mining is started.")))
  }
}
