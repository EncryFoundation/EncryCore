package encry.cli

import akka.actor.{Actor, ActorRef}
import encry.cli.CliListener.StartListening
import encry.cli.commands._
import encry.settings.EncryAppSettings
import scorex.core.utils.ScorexLogging

import scala.collection.mutable
import scala.io.StdIn

case class CliListener(nodeViewHolderRef: ActorRef, settings: EncryAppSettings) extends Actor with ScorexLogging {

  val prompt = "$> "

  val commands: mutable.HashMap[String,mutable.HashMap[String, Command]] = mutable.HashMap.empty

  commands.update("node", mutable.HashMap(
    "-stop" -> NodeShutdown
    ))

  commands.update("app", mutable.HashMap(
    "-help" -> Help
  ))

  commands.update("wallet", mutable.HashMap(
    "-addKey" -> KeyManagerAddKey,
    "-init" -> InitKeyStorage,
    "-getKeys" -> KeyManagerGetKeys,
    "-balance" -> GetBalance,
    "-sendTx" -> Transfer
    ))

  override def receive: Receive = {

    case StartListening =>
      Iterator.continually(StdIn.readLine(prompt)).takeWhile(!_.equals("quit")).foreach { input =>
        commands.get(parseCommand(input).head) match {
          case Some(value) =>
            parseCommand(input).slice(1, parseCommand(input).length).foreach { command =>
              value.get(command.split("=").head) match {
                case Some(cmd) =>
                  cmd.execute(nodeViewHolderRef, command.split("="), settings).get
                case None =>
                  println("Unsupported command. Type 'app -help' to get commands list")
                  log.debug("Unsupported command")
              }
          }
          case None =>
            println("Unsupported command. Type 'app -help' to get commands list")
        }
      }
  }

  private def parseCommand(command: String): Seq[String] = {
    val commandsSeq = command.split(" ").toSeq
    commandsSeq
  }

}

object CliListener {

  case object StartListening
}
