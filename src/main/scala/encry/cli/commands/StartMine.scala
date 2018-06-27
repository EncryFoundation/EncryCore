package encry.cli.commands

import encry.EncryApp._
import encry.cli.Response
import encry.local.miner.EncryMiner.EnableMining
import encry.settings.EncryAppSettings

import scala.concurrent.Future

object StartMine extends Command {

  import encry.local.miner.EncryMiner.StartMining

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    miner ! EnableMining
    miner ! StartMining
    Future(Some(Response("Mining is started.")))
  }
}
