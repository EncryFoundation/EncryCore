package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.view.history.History
import encry.view.wallet.EncryWallet
import encry.api.http.DataHolderForApi.GetDataFromPresentView
import encry.view.state.UtxoState
import org.encryfoundation.common.utils.Algos
import scala.concurrent.Future

object PrintPubKeys extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings, dataHolder: ActorRef): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ?
      GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Response]] { view =>
        Some(Response(view.vault.publicKeys.foldLeft("")((str, k) => str + Algos.encode(k.pubKeyBytes) + "\n")))
      }).mapTo[Option[Response]]
  }
}
