package encry.cli.commands

import akka.actor.ActorRef
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings

trait Command {

  def execute(nodeViewHolderRef: ActorRef, args: List[Ast.Param], settings: EncryAppSettings): Option[Response]
}
