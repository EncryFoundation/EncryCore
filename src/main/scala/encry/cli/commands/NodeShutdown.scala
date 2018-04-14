package encry.cli.commands
import akka.actor.ActorRef
import encry.EncryApp
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings

object NodeShutdown extends Command {

  override def execute(nodeViewHolderRef: ActorRef,
                       args: Command.Args, settings: EncryAppSettings): Option[Response] = {
    EncryApp.forceStopApplication()
    None
  }
}
