package encry.cli

import akka.actor.{Actor, ActorRef}
import akka.pattern._
import akka.util.Timeout
import encry.cli.CliListener.StartListening
import encry.cli.commands._
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.utils.ScorexLogging

import scala.collection.mutable
import scala.io.StdIn

case class CliListener(nodeViewHolderRef: ActorRef, settings: EncryAppSettings) extends Actor with ScorexLogging {

  val commands: mutable.HashMap[String,mutable.HashMap[String, Command]] = mutable.HashMap.empty

  commands.update("node", mutable.HashMap(
    "-stop" -> NodeShutdown
    ))

  commands.update("wallet", mutable.HashMap(
    "-addKey" -> KeyManagerAddKey,
    "-init" -> KeyManagerInit,
    "-getKeys" -> KeyManagerGetKeys,
    "-balance" -> GetBalance
    ))

  override def receive: Receive = {

    case StartListening =>
      Iterator.continually(StdIn.readLine()).takeWhile(!_.equals("quit")).foreach { input =>
        commands.get(parseCommand(input).head) match {
          case Some(value) =>
            parseCommand(input).slice(1, parseCommand(input).length).foreach { command =>
              value.get(command.split("=").head) match {
                case Some(cmd) =>
                  implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
                  nodeViewHolderRef ?
                    GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Unit] { view =>
                      cmd.execute(view, command.split("="))
                    }
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
