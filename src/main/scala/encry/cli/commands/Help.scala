package encry.cli.commands

import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder

import scala.util.Try

object Help extends Command {

  override def execute(view: NodeViewHolder.CurrentView[EncryHistory,
    UtxoState, EncryWallet, EncryMempool], args: Array[String]): Try[Unit] = Try{

    println(
      """
        |Usage: [GROUP_NAME] [COMMAND]=[ARGUMENT]
        |
        |Group name   Command   Argument   Meaning
        |----------------------------------------------------------------
        |node         -stop     None       Stop node
        |wallet       -getKeys  None       Show wallet keys
        |-//-         -init     Seed       Init storage with seed
        |-//-         -addKey   None       Add key to storage
        |-//-         -balance  None       Show balance of current wallet
        |app          -help     None       Show all supported commands
      """
        .stripMargin)
  }
}
