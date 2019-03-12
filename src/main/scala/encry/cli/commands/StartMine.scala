package encry.cli.commands

import akka.actor.ActorSelection
import encry.EncryApp._
import encry.cli.Response
import encry.local.miner.Miner.EnableMining
import encry.settings.EncryAppSettings
import scala.concurrent.Future

object StartMine extends Command {

  import encry.local.miner.Miner.StartMining

  val miner: ActorSelection = system.actorSelection("/user/miner")
  val nodeViewSynchronizer: ActorSelection = system.actorSelection("/user/nodeViewSynchronizer")

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    miner! EnableMining
    miner ! StartMining
    nodeViewSynchronizer ! StartMining
    Future(Some(Response("Mining is started.")))
  }
}
