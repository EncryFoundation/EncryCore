package encry.storage.levelDb.forksTree
import com.google.common.primitives.Longs
import encry.modifiers.NodeViewModifier
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.Box.Amount
import encry.utils.BalanceCalculator
import encry.utils.CoreTaggedTypes.ModifierId
import org.encryfoundation.common.Algos
import org.iq80.leveldb.DB
import cats.syntax.semigroup._
import cats.instances.all._

case class WalletForksTree(override val db: DB) extends ForksTree[WalletDiff] {

  import WalletForksTree._

  def id: ModifierId = modifiersTree.modifierId

  override def add(modifier: NodeViewModifier): Unit = {
    logger.info(s"Applying mod: ${Algos.encode(modifier.id)} with height ${modifier.asInstanceOf[Block].header.height}")
    val diffs = WalletForksTree.getDiffs(modifier)
    logger.info(s"Diff qty is: ${diffs.length}")
    logger.info(s"Balance change are: ${diffs.tail.foldLeft(diffs.head)(_ ++ _).balanceChanges}")
    applyDiff(diffs.tail.foldLeft(diffs.head)(_ ++ _))
    val newModifiersTree = ForksTreeNode(Seq(modifiersTree), modifier.id, diffs)
    modifiersTree.setParent(newModifiersTree)
    modifiersTree = newModifiersTree
    logger.info(s"Current tree nod id is: ${Algos.encode(id)}")
    logger.info(s"Balance tree: $getBalances")
  }

  def applyDiff(diff: WalletDiff): Unit = {
    diff.boxesToRemove.foreach(key => db.delete(key))
    diff.boxesToAdd.foreach(box => db.put(box.id, box.bytes))
    val previousBalances: Map[String, Amount] = getBalances
    logger.info(s"Previous balance: ${previousBalances.map{case (tokenId, balance) => s"${tokenId} -> $balance"}}")
    val newBalances: Map[String, Amount] = diff.balanceChanges |+| previousBalances
    db.put(BalancesKey, newBalances.foldLeft(Array.empty[Byte]) { case (acc, (id, balance)) =>
      acc ++ Algos.decode(id).get ++ Longs.toByteArray(balance)
    })
  }

  def getBalances: Map[String, Amount] = {
    val balances = db.get(BalancesKey)
    if (balances == null) Map.empty
    else balances
      .sliding(40, 40)
      .map(ch => Algos.encode(ch.take(32)) -> Longs.fromByteArray(ch.takeRight(8)))
      .toMap
  }
}

object WalletForksTree {

  val BalancesKey: Array[Byte] = Algos.hash("balances")

  def apply(db: DB): WalletForksTree = {
    val tree = new WalletForksTree(db)
    db.put(BalancesKey, Array.emptyByteArray)
    tree
  }

  def getDiffs(modifier: NodeViewModifier): Seq[WalletDiff] = modifier match {
    case block: Block => block.payload.transactions.flatMap(getDiffs)
    case tx: Transaction =>
      val toDelete = tx.inputs.map(_.boxId)
      val toAdd = tx.newBoxes.toList
      val balances = BalanceCalculator.balanceSheet(toAdd)
      Seq(WalletDiff(toDelete, toAdd, balances.map{case (tokenId, value) => Algos.encode(tokenId) -> value}))
    case _ => Seq.empty
  }
}
