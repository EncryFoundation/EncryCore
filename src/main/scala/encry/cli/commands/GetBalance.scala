package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.utils.Algos
import scala.concurrent.Future

object GetBalance extends Command {

  /**
    * Command "wallet balance"
    */
  override def execute(args: Command.Args, settings: EncryAppSettings, dataHolder: ActorRef, nodeId: Array[Byte], ntp: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ?
      GetDataFromCurrentView[History, UtxoState, EncryWallet, Option[Response]] { view =>
        Option(Response(
          {
            val balance: String =
              view.vault.getBalances.foldLeft("")((str, tokenInfo) =>
                if (tokenInfo._1._2 != Algos.encode(settings.constants.IntrinsicTokenId))
                  str.concat(s"TokenID(${tokenInfo._1._2}) for key ${tokenInfo._1._1} : ${tokenInfo._2}\n")
                else str.concat(s"TokenID(${tokenInfo._1._2}) for key ${tokenInfo._1._1} : ${BigDecimal(tokenInfo._2) / 100000000}\n")
            )
            if (balance.length == 0) "0" else balance
          }
        ))
      }).mapTo[Option[Response]]
  }

}