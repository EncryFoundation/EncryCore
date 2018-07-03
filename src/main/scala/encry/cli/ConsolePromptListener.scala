package encry.cli

import akka.actor.{Actor, ActorSystem}
import akka.stream.{ActorAttributes, ActorMaterializer}
import akka.stream.scaladsl.{Sink, Source}
import encry.EncryApp
import encry.cli.commands._

import scala.concurrent.ExecutionContext.Implicits.global
import encry.EncryApp.settings
import encry.utils.ScorexLogging

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class ConsolePromptListener extends Actor with ScorexLogging {

  import ConsolePromptListener._
  implicit val materializer = ActorMaterializer()
//  implicit val executionContext: ExecutionContext = context.system.dispatchers.lookup("miner-dispatcher")

  override def receive: Receive = {
    case StartListening =>
//            Iterator.continually(scala.io.StdIn.readLine(prompt)).foreach { input =>
//              InputParser.parse(input) match {
//                case Success(command) =>
//                  getCommand(command.category.name, command.ident.name) match {
//                    case Some(cmd) =>
//                      cmd.execute(Command.Args(command.params.map(p => p.ident.name -> p.value).toMap), settings)
//                        .map {
//                          case Some(x) => print(x.msg + s"\n$prompt")
//                          case None =>
//                        }
//                    case None => println("Unsupported command. Type 'app help' to get commands list")
//                  }
//                case Failure(_) => println("Bad input")
//              }
//            }

      Iterator.continually(scala.io.StdIn.readLine(prompt))
        .foreach { input =>
        val parsed = InputParser.parse(input)
        if (parsed.isFailure) println("Bad input")
        parsed.foreach { command =>
          val cmd = getCommand(command.category.name, command.ident.name)
          if (cmd.isEmpty) println("Unsupported command. Type 'app help' to get commands list")
          cmd.foreach{ c =>
            val request = c.executeRequest(Command.Args(command.params.map(p => p.ident.name -> p.value).toMap), settings)
            if (request == None)
              c.execute(Command.Args(command.params.map(p => p.ident.name -> p.value).toMap), settings)
              .onComplete{case Success(Some(x)) => print(x.msg + s"\n$prompt") }
            else {
              println("Sending Ping")
              EncryApp.nodeViewHolder ! "PING"
            }
          }
        }
      }

      println("Done startListening")
    case Some(Response(res)) => print(res + s"\n.$prompt")
    case s => println(s"Any: $s")
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