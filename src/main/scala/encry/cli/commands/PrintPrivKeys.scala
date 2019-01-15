package encry.cli.commands

import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.view.mempool.Mempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import encry.EncryApp._
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.encryfoundation.common.Algos
import scala.concurrent.Future

//TODO This cmd is unsafe.
object PrintPrivKeys extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nodeViewHolder ?
      GetDataFromCurrentView[UtxoState, EncryWallet, Mempool, Option[Response]] { view =>
        Some(Response(view.vault.accountManager.accounts.foldLeft("")((str, k) => str + Algos.encode(k.privKeyBytes)) + "\n"))
      }).mapTo[Option[Response]]
  }
}
