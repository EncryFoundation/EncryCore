package encry.cli.commands
import akka.actor.ActorRef
import encry.EncryApp
import encry.settings.EncryAppSettings

import scala.util.Try

object NodeShutdown extends Command {

  override def execute(nodeViewHolderRef: ActorRef, args: Array[String], settings: EncryAppSettings): Try[Unit] =
    Try(EncryApp.forceStopApplication())
}
