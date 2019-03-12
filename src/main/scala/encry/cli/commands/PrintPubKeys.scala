package encry.cli.commands

import akka.actor.ActorSelection
import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.mempool.Mempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import encry.EncryApp._
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.encryfoundation.common.Algos
import scala.concurrent.Future

object PrintPubKeys extends Command {

  val nvh: ActorSelection = system.actorSelection("/user/nodeViewHolder")

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nvh ? GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, Mempool, Option[Response]] { view =>
      Some(Response(view.vault.publicKeys.foldLeft("")((str, k) => str + Algos.encode(k.pubKeyBytes)) + "\n"))
    }).mapTo[Option[Response]]
  }
}
