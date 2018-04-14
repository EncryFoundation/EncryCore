package encry.cli.commands

import akka.actor.ActorRef
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings

trait Command {

  def execute(nodeViewHolderRef: ActorRef, args: Command.Args, settings: EncryAppSettings): Option[Response]
}

object Command {

  case class Args(args: Map[String, Ast.Value]) {

    def requireArg[VT <: Ast.Value](n: String): VT = args.get(n).map {
      case vt: VT@unchecked => vt
      case _ => throw new Error("Wrong argument type.")
    }.getOrElse(throw new Error(s"Argument $n not found."))

    /**
      * def getOrElse - try to get v by key. Return v if v exist and v is instance of default
      * return default if v doesn't exist or v is not instance of default
      * @param key
      * @param default
      * @tparam VT
      * @return
      */
    def getOrElse[VT <: Ast.Value](key: String, default: => VT): VT =
      args.get(key) match {
      case Some(v) => v match {
        case vt: VT@unchecked => vt
        case _ => default
      }
      case None => default
    }
  }
}
