package encry.cli.commands

import akka.actor.ActorRef
import akka.util.Timeout
import akka.pattern._
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.crypto.encode.Base58

import scala.util.Try

object PrintPrivateKeys extends Command {

  override def execute(nodeViewHolderRef: ActorRef, args: Array[String], settings: EncryAppSettings): Try[Unit] = Try {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    nodeViewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Unit] { view =>
        view.vault.keyManager.keys.foreach(
          a => println(Base58.encode(a.privKeyBytes))
        )
      }
  }
}
