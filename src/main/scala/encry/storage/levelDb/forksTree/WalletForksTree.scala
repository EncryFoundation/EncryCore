package encry.storage.levelDb.forksTree
import encry.modifiers.NodeViewModifier
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction

class WalletForksTree extends ForksTree {

  override def add(modifier: NodeViewModifier): Unit = ???
}

object WalletForksTree {

  def getDiffs(modifier: NodeViewModifier, ): Seq[Diff] = modifier match {
    case block: Block =>
      Seq.empty
    case tx: Transaction =>
    case
    case _ => Seq.empty
  }
}
