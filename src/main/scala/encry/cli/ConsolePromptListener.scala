package encry.cli

import akka.actor.Actor
import akka.stream.ActorMaterializer
import encry.EncryApp
import encry.EncryApp.settings
import encry.cli.commands._
import encry.utils.EncryLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success


class ConsolePromptListener extends Actor with EncryLogging {

  import ConsolePromptListener._
  implicit val materializer = ActorMaterializer()

  override def receive: Receive = {
    case StartListening =>
      Iterator.continually(scala.io.StdIn.readLine(prompt))
        .foreach { input =>
        val parsed = InputParser.parse(input)
        if (parsed.isFailure) println("Bad input")
        parsed.foreach { command =>
          val cmd = getCommand(command.category.name, command.ident.name)
          if (cmd.isEmpty) println("Unsupported command. Type 'app help' to get commands list")
          cmd.foreach{ c =>
            val request = c.executeRequest(Command.Args(command.params.map(p => p.ident.name -> p.value).toMap), settings)
            if (request == LocalCommand) c.execute(Command.Args(command.params.map(p => p.ident.name -> p.value).toMap), settings)
              .onComplete{case Success(Some(x)) => print(s"${x.msg}\n$prompt") }
            else EncryApp.nodeViewHolder ! request
          }
        }
      }
    case Some(Response(res)) => print(res + s"\n.$prompt")
  }
}

object ConsolePromptListener {

  case object StartListening

  val prompt = "$> "

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