package encry.cli.commands

import akka.actor.ActorRef
import encry.cli.Response
import encry.settings.EncryAppSettings

trait Command {

  def execute(nodeViewHolderRef: ActorRef, args: Array[String], settings: EncryAppSettings): Option[Response]
}
