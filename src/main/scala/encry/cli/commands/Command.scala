package encry.cli.commands

import akka.actor.ActorRef
import encry.cli.{Ast, Response}
import encry.settings.{Algos, EncryAppSettings}
import scala.concurrent.Future

trait Command {

  def execute(nodeViewHolderRef: ActorRef, args: Command.Args, settings: EncryAppSettings): Future[Option[Response]]

}

object Command {

  case class Args(args: Map[String, Ast.Value]) {

    def requireArg[VT <: Ast.Value](n: String): VT = args.get(n).map {
      case vt: VT@unchecked => vt
      case _ => throw new Error("Wrong argument type.")
    }.getOrElse(throw new Error(s"Argument $n not found."))

    def requireArgOrElse[VT <: Ast.Value](key: String, default: => VT): VT = args.get(key).map {
      case vt: VT@unchecked => vt
      case _ => default
    }.getOrElse(default)
  }
}
