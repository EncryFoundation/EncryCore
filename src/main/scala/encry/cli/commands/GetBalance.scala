package encry.cli.commands
import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder.GetDataFromCurrentView

import scala.concurrent.Await
import scala.concurrent.duration._

object GetBalance extends Command {

  override def execute(nodeViewHolderRef: ActorRef,
                       args: Array[String], settings: EncryAppSettings): Option[Response] = {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    Await.result((nodeViewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[Response]] { view =>
        Some(Response(view.vault.balance.toString))
      }).mapTo[Option[Response]], 5.second)
  }
}
