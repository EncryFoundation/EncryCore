package encry.cli

import akka.actor.{Actor, Props}
import encry.cli.commands._
import encry.settings.EncryAppSettings

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class ConsoleListener(settings: EncryAppSettings) extends Actor {

  import ConsoleListener._

  override def receive: Receive = {
    case StartListening =>
      Iterator.continually(scala.io.StdIn.readLine(prompt)).foreach { input =>
        InputParser.parse(input) match {
          case Success(command) =>
            getCommand(command.category.name, command.ident.name) match {
              case Some(cmd) =>
                cmd.execute(Command.Args(command.params.map(p => p.ident.name -> p.value).toMap), settings)
                  .map {
                    case Some(x) => print(x.msg + s"\n$prompt")
                    case None =>
                  }
              case None => println("Unknown command. Type 'app help' to see command list.")
            }
          case Failure(_) =>
        }
      }
  }
}

object ConsoleListener {

  case object StartListening

  val prompt = "$> "

  def getCommand(cat: String, cmd: String): Option[Command] = cmdDictionary.get(cat).flatMap(_.get(cmd))

  private val nodeCmds = Map("node" -> Map(
    "shutdown"    -> NodeShutdown,
    "stopMining"  -> StopMine,
    "startMining" -> StartMine,
  ))

  private val appCmds = Map("app" -> Map(
    "help" -> Help
  ))

  private val settingsCmds = Map("settings" -> Map(
    "addPeer" -> AddPeer,
    "removeFromBlackList" -> RemoveFromBlackList
  ))

  private val walletCmds = Map("wallet" -> Map(
    "addrs"     -> PrintAddresses,
    "createKey" -> CreateKey,
    "pubKeys"   -> PrintPubKeys,
    "balance"   -> GetBalance,
    "transfer"  -> Transfer,
    "privKeys"  -> PrintPrivKeys, //Todo delete
    "fromSeed"  -> CreateAccountFromSeed
  ))

  val cmdDictionary: Map[String, Map[String, Command]] =
    ConsoleListener.nodeCmds ++ ConsoleListener.appCmds ++ ConsoleListener.walletCmds ++ ConsoleListener.settingsCmds

  def props(settings: EncryAppSettings): Props = Props(new ConsoleListener(settings))
}