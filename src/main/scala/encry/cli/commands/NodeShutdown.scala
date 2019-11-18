package encry.cli.commands

import akka.actor.ActorRef
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.api.http.DataHolderForApi.ShutdownNode
import encry.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object NodeShutdown extends Command {

  /**
    * Command "node shutdown"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    dataHolder ! ShutdownNode
    Future(None)
  }
}
