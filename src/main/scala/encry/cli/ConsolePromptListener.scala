package encry.cli

import akka.actor.{Actor, ActorRef}
import encry.cli.commands._
import encry.settings.EncryAppSettings
import fastparse.core.Parsed
import jline.console.ConsoleReader
import scorex.core.utils.ScorexLogging
import fastparse.all._

case class ConsolePromptListener(nodeViewHolderRef: ActorRef, settings: EncryAppSettings)
  extends Actor with ScorexLogging {

  import ConsolePromptListener._

  private val prompt = "$> "

  private val reader = new ConsoleReader()

  override def receive: Receive = {
    case StartListening =>
      Iterator.continually(reader.readLine(prompt)).takeWhile(!_.equals("quit")).foreach { input =>
        (InputParser.commandP ~ End).parse(input) match {
          case Parsed.Success(command, _) =>
            getCommand(command.category.name, command.ident.name) match {
              case Some(cmd) =>
                println(
                  cmd.execute(
                    nodeViewHolderRef,
                    Command.Args(command.params.map(p => p.ident.name -> p.value ).toMap),
                    settings
                  ).map(_.msg).getOrElse("")
                )
              case _ =>
                println("Unsupported command. Type 'app help' to get commands list")
            }
          case _ =>
            println("Bad input")
        }
      }
  }
}

object ConsolePromptListener {

  case object StartListening

  def getCommand(cat: String, cmd: String): Option[Command] = cmdDictionary.get(cat).flatMap(_.get(cmd))

  private val nodeCmds = Map("node" -> Map(
    "shutdown" -> NodeShutdown
  ))

  private val appCmds = Map("app" -> Map(
    "help" -> Help
  ))

  private val walletCmds = Map("wallet" -> Map(
    "addrs" -> PrintMyAddrs,
    "addKey" -> AddKey,
    "init" -> InitKeyStorage,
    "pubKeys" -> PrintPubKeys,
    "balance" -> GetBalance,
    "transfer" -> Transfer
  ))

  val cmdDictionary: Map[String, Map[String, Command]] =
    ConsolePromptListener.nodeCmds ++ ConsolePromptListener.appCmds ++ ConsolePromptListener.walletCmds
}
