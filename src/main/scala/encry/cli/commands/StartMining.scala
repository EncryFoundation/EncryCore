package encry.cli.commands

import akka.actor.ActorRef

import scala.concurrent.ExecutionContext.Implicits.global
import encry.api.http.DataHolderForApi.StartMiner
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider

import scala.concurrent.Future

object StartMining extends Command {

  /**
    * Command "node startMining"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    dataHolder ! StartMiner
    Future.successful(Some(Response("Mining is started.")))
  }
}
