package encry.cli.commands

import akka.pattern._
import akka.util.Timeout
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import encry.EncryApp._
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scala.concurrent.Future
import scala.util.Try

object CreateKey extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = Try {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    nodeViewHolder ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Unit] { view =>
        if (view.vault.accountManager.accounts.isEmpty) view.vault.accountManager.mandatoryAccount
        else view.vault.accountManager.createAccount(None)
      }
  }.map(_ => Future(Some(Response("OK")))).getOrElse(Future(Some(Response("Operation failed"))))
}
