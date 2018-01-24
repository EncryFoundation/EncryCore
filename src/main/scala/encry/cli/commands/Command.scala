package encry.cli.commands

import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder.CurrentView

import scala.util.Try

trait Command {

  def execute(view: CurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool], args: Array[String]): Try[Unit]
}
