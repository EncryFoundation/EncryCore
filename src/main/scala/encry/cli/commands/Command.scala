package encry.cli.commands

import akka.util.Timeout
import encry.EncryApp.nodeViewHolder
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings
import akka.pattern._


import scala.concurrent.Future

trait Command {
  def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]]
  def executeRequest(args: Command.Args, settings: EncryAppSettings): Any = LocalCommand
}

object LocalCommand extends ViewCommand

trait ViewCommand extends Command {
  override def executeRequest(args: Command.Args, settings: EncryAppSettings): Any = this
  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nodeViewHolder ? executeRequest(args, settings)).mapTo[Option[Response]]
  }
}

object Command {

  case class Args(args: Map[String, Ast.Value]) {

    def requireArg[VT <: Ast.Value](n: String): VT = args.get(n).map {
      case vt: VT@unchecked => vt
      case _ => throw new Exception("Wrong argument type.")
    }.getOrElse(throw new Exception(s"Argument $n not found."))

    def requireArgOrElse[VT <: Ast.Value](key: String, default: => VT): VT = args.get(key).map {
      case vt: VT@unchecked => vt
      case _ => default
    }.getOrElse(default)
  }
}
