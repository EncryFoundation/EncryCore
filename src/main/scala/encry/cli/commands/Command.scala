package encry.cli.commands

import akka.actor.ActorRef
import akka.util.Timeout
import encry.settings.EncryAppSettings

import scala.util.Try

trait Command {
  def execute(nodeViewHolderRef: ActorRef, args: Array[String], settings: EncryAppSettings): Try[Unit]
}
