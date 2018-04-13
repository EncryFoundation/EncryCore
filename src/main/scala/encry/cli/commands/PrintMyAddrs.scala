package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView

import scala.concurrent.Await
import scala.concurrent.duration._

object PrintMyAddrs extends Command {

  override def execute(nodeViewHolderRef: ActorRef,
                       args: List[Ast.Param], settings: EncryAppSettings): Option[Response] = {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    Await.result((nodeViewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[Response]] { view =>
        Some(Response(view.vault.keyManager.keys.foldLeft("")((str, k) => str + k.publicImage.address + "\n")))
      }).mapTo[Option[Response]], 5.second)
  }
}
