package encry.cli.commands

import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.mempool.Mempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import encry.EncryApp._
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scala.concurrent.Future

object PrintAddresses extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nodeViewHolder ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, Mempool, Option[Response]] { view =>
        Some(Response(view.vault.publicKeys.foldLeft("") { (str, k) =>
          str + s"Pay2PubKeyAddress : ${k.address.address} , Pay2ContractHashAddress : ${k.address.p2ch.address}" + "\n"
        }))
      }).mapTo[Option[Response]]
  }
}
