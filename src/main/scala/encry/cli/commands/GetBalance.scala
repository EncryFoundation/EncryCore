package encry.cli.commands

import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scala.concurrent.Future
import encry.EncryApp._

object GetBalance extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nodeViewHolder ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, Option[Response]] { view =>
        Option(Response(
          {
            val balance: String =
              view.vault.getBalances.foldLeft("")((str, tokenInfo) =>
                str.concat(s"TokenID(${tokenInfo._1}) : ${tokenInfo._2}\n"))
            if (balance.length == 0) "0" else balance
          }
        ))
      }).mapTo[Option[Response]]
  }
}
