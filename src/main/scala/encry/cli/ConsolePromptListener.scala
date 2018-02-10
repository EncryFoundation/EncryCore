package encry.cli

import akka.actor.{Actor, ActorRef}
import encry.cli.ConsolePromptListener.StartListening
import encry.cli.commands._
import encry.settings.EncryAppSettings
import scorex.core.utils.ScorexLogging

import scala.collection.mutable
import scala.io.StdIn

case class ConsolePromptListener(nodeViewHolderRef: ActorRef, settings: EncryAppSettings) extends Actor with ScorexLogging {

  val prompt = "$> "

  val commands: mutable.HashMap[String,mutable.HashMap[String, Command]] = mutable.HashMap.empty

  commands.update("node", mutable.HashMap(
    "-shutdown" -> NodeShutdown
  ))

  commands.update("app", mutable.HashMap(
    "-help" -> Help
  ))

  commands.update("pki", mutable.HashMap(
    "-addPubKeyInfo" -> AddPubKeyInfo
  ))

  commands.update("wallet", mutable.HashMap(
    "-addrs" -> PrintMyAddrs,
    "-addKey" -> AddKey,
    "-init" -> InitKeyStorage,
    "-pubKeys" -> PrintPubKeys,
    "-balance" -> GetBalance,
    "-transfer" -> Transfer
  ))

  override def receive: Receive = {

    case StartListening =>
      Iterator.continually(StdIn.readLine(prompt)).takeWhile(!_.equals("quit")).foreach { input =>
        commands.get(parseCommand(input).head) match {
          case Some(value) =>
            parseCommand(input).slice(1, parseCommand(input).length).foreach { command =>
              value.get(command.split("=").head) match {
                case Some(cmd) =>
                  println(cmd.execute(nodeViewHolderRef, command.split("="), settings).map(_.inner).getOrElse(""))
                case None =>
                  println("Unsupported command. Type 'app -help' to get commands list")
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

object ConsolePromptListener {

  case object StartListening
}
