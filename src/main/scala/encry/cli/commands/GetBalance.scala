package encry.cli.commands
import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.settings.{Algos, EncryAppSettings}
import encry.utils.BalanceCalculator
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView

import scala.concurrent.Await
import scala.concurrent.duration._

object GetBalance extends Command {

  override def execute(nodeViewHolderRef: ActorRef,
                       args: Command.Args, settings: EncryAppSettings): Option[Response] = {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    Await.result((nodeViewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[Response]] { view =>
        Some(Response(
          BalanceCalculator.balanceMapToString(
            view.vault.tokenBalance.map(key => (Algos.encode(key._1), key._2)).toMap,
            view.vault.encryBalance
          )
        ))
      }).mapTo[Option[Response]], 5.second)
  }
}
