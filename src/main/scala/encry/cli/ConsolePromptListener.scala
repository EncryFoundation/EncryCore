package encry.cli

import akka.actor.Actor
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import encry.EncryApp
import encry.EncryApp.settings
import encry.cli.commands._
import encry.utils.ExtUtils._
import encry.utils.Logging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success


class ConsolePromptListener extends Actor with Logging {

  import ConsolePromptListener._

  implicit val materializer = ActorMaterializer()

  override def receive: Receive = {
    case StartListening =>
      Source.fromIterator(() => Iterator.continually(scala.io.StdIn.readLine(prompt)))
        .runForeach{ input =>
          InputParser.parse(input).iapply(parsed => if (parsed.isFailure) println("Bad input"))
            .foreach { command =>
              getCommand(command.category.name, command.ident.name)
                .iapply(cmd => if (cmd.isEmpty) println("Unsupported command. Type 'app help' to get commands list"))
                .foreach{ c =>
                  val request = c.executeRequest(Command.Args(command.params.map(p => p.ident.name -> p.value).toMap), settings)
                  if (request == LocalCommand) c.execute(Command.Args(command.params.map(p => p.ident.name -> p.value).toMap), settings)
                    .onComplete{case Success(Some(x)) => print(s"${x.msg}\n$prompt") }
                  else EncryApp.nodeViewHolder ! request
                }
            }
          }

    case Some(Response(res)) => print(res + s"\n$prompt")
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