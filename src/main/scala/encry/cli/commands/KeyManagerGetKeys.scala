package encry.cli.commands

import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder
import scorex.crypto.encode.Base58

import scala.util.Try

object KeyManagerGetKeys extends Command {

  override def execute(view: NodeViewHolder.CurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool],
                       args: Array[String]): Try[Unit] = Try {
    view.vault.keyStorage.keys.foreach(
      a => println(Base58.encode(a.publicKeyBytes))
    )
  }
}
