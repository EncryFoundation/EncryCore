package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import encry.view.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

object CreateKey extends Command {

  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       ncRef: ActorRef,
                       nvhRef: ActorRef,
                       minerRef: ActorRef,
                       nvshRef: ActorRef,
                       mempoolRef: ActorRef): Future[Option[Response]] = Try {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    nvhRef ? GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, Unit] { view =>
      if (view.vault.accountManager.accounts.isEmpty) view.vault.accountManager.mandatoryAccount
      else view.vault.accountManager.createAccount(None)
    }
  }.map(_ => Future(Some(Response("OK")))).getOrElse(Future(Some(Response("Operation failed"))))
}
