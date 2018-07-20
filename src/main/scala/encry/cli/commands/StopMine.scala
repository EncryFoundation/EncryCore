package encry.cli.commands

import encry.EncryApp._
import encry.cli.Response
import encry.settings.EncryAppSettings

import scala.concurrent.Future

object StopMine extends Command {

  import encry.local.miner.EncryMiner.DisableMining

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    miner ! DisableMining
    nodeViewSynchronizer ! DisableMining
    Future(Some(Response("Mining is stopped.")))
  }
}
