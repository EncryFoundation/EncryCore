package encry.cli

import akka.actor.Actor
import akka.util.Timeout
import encry.EncryApp
import encry.settings.{Algos, EncryAppSettings}
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import akka.actor.ActorRef
import akka.pattern._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success

object ConsoleActor {

  sealed trait Command

  case class Order(rawCommand: String) extends Command

}

class ConsoleActor(nvh: ActorRef, miner: ActorRef, settings: EncryAppSettings) extends Actor {

  import ConsoleActor._

  implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)

  override def receive: Receive = {
    case Order("app help") => println(helper)
    case Order("shutdown") => EncryApp.forceStopApplication()
    case Order("wallet balance") =>
      val response: Future[Option[Response]] =
        (nvh ? GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[Response]] { view =>
          Option(Response(
            view.vault.getBalances.foldLeft("")((str, tokenInfo) => str.concat(s"TokenID(${Algos.encode(tokenInfo._1)}) : ${tokenInfo._2}\n"))
          ))
        }).mapTo[Option[Response]]
      response.map {
        case Some(x) => println(x.msg)
        case None =>
      }
    case Order("wallet addrs") =>
    case Order("wallet addKey") =>
    case Order("wallet init") =>
    case Order("wallet pubKeys") =>
    case Order("wallet transfer") =>
    case _ => println("unknown command, try")
  }

  val helper: String =
    """
      |Usage: [GROUP_NAME] [COMMAND] -[ARGUMENT_1]=[VAL_1] -[ARGUMENT_2]=[VAL_2]
      |
      |Group name    Command          Argument       Meaning
      |--------------------------------------------------------------------------------
      |node          shutdown         None           Shutdown the node
      |wallet        pubKeys          None           Print available public keys
      |wallet        addrs            None           Print available addresses
      |wallet        init             seed           Init storage with seed
      |wallet        init             None           Generate new storage
      |wallet        addKey           None           Add key to storage
      |wallet        balance          None           Show balance of current wallet
      |wallet        transfer         addr, amount   Transfer `amount` to `addr`ess
      |app           help             None           Show all supported commands
    """
      .stripMargin
}

