package encry.storage.levelDb.forksTree
import encry.modifiers.NodeViewModifier
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.utils.BalanceCalculator

class WalletForksTree extends ForksTree {

  override def add(modifier: NodeViewModifier): Unit = ???
}

object WalletForksTree {

  def getDiffs(modifier: NodeViewModifier): Seq[Diff] = modifier match {
    case block: Block => block.payload.transactions.flatMap(getDiffs)
    case tx: Transaction =>
      val toDelete = tx.inputs.map(_.boxId)
      val toAdd = tx.newBoxes.toList
      val balances = BalanceCalculator.balanceSheet(toAdd)
      Seq(WalletDiff(toDelete, toAdd, balances))
    case _ => Seq.empty
  }
}
