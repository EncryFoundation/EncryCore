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
import org.encryfoundation.common.Algos
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

//TODO This cmd is unsafe.
object PrintPrivKeys extends Command {

  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       ncRef: ActorRef,
                       nvhRef: ActorRef,
                       minerRef: ActorRef,
                       nvshRef: ActorRef,
                       mempoolRef: ActorRef): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nvhRef ? GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, Option[Response]] { view =>
      Some(Response(view.vault.accountManager.accounts.foldLeft("")((str, k) =>
        str + Algos.encode(k.privKeyBytes) + "\n"))
      )
    }).mapTo[Option[Response]]
  }
}