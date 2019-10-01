package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.EncryApp._
import encry.api.http.DataHolderForApi.GetDataFromPresentView
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scala.concurrent.Future
import scala.util.Try

object CreateKey extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings, dataHolder: ActorRef): Future[Option[Response]] = Try {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    dataHolder ?
      GetDataFromPresentView[History, UtxoState, EncryWallet, Unit] { view =>
        if (view.vault.accountManager.accounts.isEmpty) view.vault.accountManager.mandatoryAccount
        else view.vault.accountManager.createAccount(None)
      }
  }.map(_ => Future(Some(Response("OK")))).getOrElse(Future(Some(Response("Operation failed"))))
}
