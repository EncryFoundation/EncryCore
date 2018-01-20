package encry.cli.commandObjects
import akka.actor.ActorRef

import scala.util.Try

object nodeStop extends Command {

  override def execute(args: String) = Try{
    System.exit(0)
  }
}
