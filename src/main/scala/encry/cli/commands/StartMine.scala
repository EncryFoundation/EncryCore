package encry.cli.commands

import akka.actor.ActorRef
import encry.cli.Response
import encry.settings.EncryAppSettings
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object StartMine extends Command {

  import encry.local.miner.EncryMiner.StartMining

  override def execute(nodeViewHolderRef: ActorRef, miner: ActorRef,
                       args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    miner ! StartMining
    Future(Some(Response("Mining is started.")))
  }
}
