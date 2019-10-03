package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.api.http.DataHolderForApi.GetDataFromPresentView
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet

import scala.concurrent.Future

object GetBalance extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings, dataHolder: ActorRef,nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ?
      GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Response]] { view =>
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
