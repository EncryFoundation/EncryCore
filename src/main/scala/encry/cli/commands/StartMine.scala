package encry.cli.commands

import akka.actor.ActorRef
import encry.cli.Response
import encry.local.miner.Miner.EnableMining
import encry.settings.EncryAppSettings
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object StartMine extends Command {

  import encry.local.miner.Miner.StartMining

  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       ncRef: ActorRef,
                       nvhRef: ActorRef,
                       minerRef: ActorRef,
                       nvshRef: ActorRef,
                       mempoolRef: ActorRef): Future[Option[Response]] = {
    minerRef ! EnableMining
    minerRef ! StartMining
    nvshRef ! StartMining
    Future(Some(Response("Mining is started.")))
  }
}
