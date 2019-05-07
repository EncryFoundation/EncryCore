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

object GetBalance extends Command {

  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       ncRef: ActorRef,
                       nvhRef: ActorRef,
                       minerRef: ActorRef,
                       nvshRef: ActorRef,
                       mempoolRef: ActorRef): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nvhRef ? GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, Option[Response]] { view =>
      Option(Response({
        val balance: String =
          view.vault.getBalances.foldLeft("")((str, tokenInfo) =>
            str.concat(s"TokenID(${tokenInfo._1}) : ${tokenInfo._2}\n"))
        if (balance.length == 0) "0" else balance
      }))
    }).mapTo[Option[Response]]
  }
}
