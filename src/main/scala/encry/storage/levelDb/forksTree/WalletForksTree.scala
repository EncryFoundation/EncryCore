package encry.storage.levelDb.forksTree
import encry.modifiers.NodeViewModifier
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.EncryBaseBox
import encry.utils.BalanceCalculator
import org.encryfoundation.common.Algos
import org.iq80.leveldb.DB

case class WalletForksTree(override val db: DB) extends ForksTree {

  def addBoxesToCache(boxes: Seq[EncryBaseBox]): Unit = {
    val batch = db.createWriteBatch()
    boxes
      .map(box => (Algos.decode("diffCache").get ++ box.id) -> box.bytes)
      .foreach{case (boxId, boxBytes) => batch.put(boxId, boxBytes)}
    db.write(batch)
  }

  override def add(modifier: NodeViewModifier): Unit = {

  }
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
