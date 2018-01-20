package encry.cli.commandObjects

import akka.actor.ActorRef

import scala.util.Try

trait Command {

  def execute(args : String) : Try[Unit]

}
