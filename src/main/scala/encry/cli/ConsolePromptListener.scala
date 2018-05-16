package encry.cli

import akka.actor.{Actor, ActorRef}
import encry.cli.commands._
import encry.settings.EncryAppSettings
import jline.console.ConsoleReader
import scorex.core.utils.ScorexLogging
import encry.EncryApp.{nodeViewHolder, miner}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class ConsolePromptListener(settings: EncryAppSettings) extends Actor with ScorexLogging {

  import ConsolePromptListener._

  private val prompt = "$> "

  private val reader = new ConsoleReader()

  override def receive: Receive = {
    case StartListening =>
      Iterator.continually(reader.readLine(prompt)).foreach { input =>
        InputParser.parse(input) match {
          case Success(command) =>
            getCommand(command.category.name, command.ident.name) match {
              case Some(cmd) =>
                  cmd.execute(
                    nodeViewHolder,
                    miner,
                    Command.Args(command.params.map(p => p.ident.name -> p.value).toMap),
                    settings
                  ).map {
                    case Some(x) =>
                      println(x.msg)
                      print(prompt)
                    case None =>
                  }
              case _ => println("Unsupported command. Type 'app help' to get commands list")
            }
          case _ => println("Bad input")
        }
      }
  }
}

object ConsolePromptListener {

  case object StartListening

  def getCommand(cat: String, cmd: String): Option[Command] = cmdDictionary.get(cat).flatMap(_.get(cmd))

  private val nodeCmds = Map("node" -> Map(
    "shutdown" -> NodeShutdown,
    "stopMining" -> StopMine,
    "startMining" -> StartMine,
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
