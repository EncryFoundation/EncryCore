package encry.cli.commands

import akka.pattern._
import akka.util.Timeout
import encry.EncryApp._
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.view.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.EncryHistory
import encry.view.state.UtxoStateWithoutAVL
import encry.view.wallet.EncryWallet
import scala.concurrent.Future
import scala.util.Try

object CreateKey extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = Try {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    nodeViewHolder ?
      GetDataFromCurrentView[EncryHistory, UtxoStateWithoutAVL, EncryWallet, Unit] { view =>
        if (view.vault.accountManager.accounts.isEmpty) view.vault.accountManager.mandatoryAccount
        else view.vault.accountManager.createAccount(None)
      }
  }.map(_ => Future(Some(Response("OK")))).getOrElse(Future(Some(Response("Operation failed"))))
}
