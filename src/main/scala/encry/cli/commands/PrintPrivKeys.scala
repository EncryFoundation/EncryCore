package encry.cli.commands

import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.settings.{Algos, EncryAppSettings}
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import encry.EncryApp._
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.crypto.encode.Base58

import scala.concurrent.Future

//TODO This cmd is unsafe.
object PrintPrivKeys extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nodeViewHolder ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[Response]] { view =>
        Some(Response(view.vault.keyManager.keys.foldLeft("")((str, k) => str + Base58.encode(k.privKeyBytes)) + "\n"))
      }).mapTo[Option[Response]]
  }
}
