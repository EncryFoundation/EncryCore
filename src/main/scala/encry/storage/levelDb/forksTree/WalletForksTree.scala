package encry.storage.levelDb.forksTree
import com.google.common.primitives.Longs
import encry.modifiers.NodeViewModifier
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.EncryBaseBox
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.utils.BalanceCalculator
import org.encryfoundation.common.Algos
import org.iq80.leveldb.DB

case class WalletForksTree(override val db: DB) extends ForksTree[WalletDiff] {

  import WalletForksTree._

  override def add(modifier: NodeViewModifier): Unit = {
    val diffs = WalletForksTree.getDiffs(modifier)
    applyDiff(diffs.tail.foldLeft(diffs.head)(_ ++ _))
    val newModifiersTree = ForksTreeNode(Seq(modifiersTree), modifier.id, diffs)
    modifiersTree.setParent(newModifiersTree)
    modifiersTree = newModifiersTree
  }

  def applyDiff(diff: WalletDiff): Unit = {
    diff.boxesToRemove.foreach(key => db.delete(key))
    diff.boxesToAdd.foreach(box => db.put(box.id, box.bytes))
    val previousBalances: Map[TokenId, Amount] = getBalances
    val newBalances: Map[Array[Byte], Amount] = diff.balanceChanges.foldLeft(Map.empty[Array[Byte], Long]) {
      case (balances, balanceToken) => balances.updated(balanceToken._1,
        previousBalances.getOrElse(balanceToken._1, 0L) + balanceToken._2)
    }
    db.put(BalancesKey, newBalances.foldLeft(Array.empty[Byte]) { case (acc, (id, balance)) =>
      acc ++ id ++ Longs.toByteArray(balance)
    })
  }

  def getBalances: Map[TokenId, Amount] = {
    val balances = db.get(BalancesKey)
    if (balances == null) Map.empty
    else balances
      .sliding(40, 40)
      .map(ch => ch.take(32) -> Longs.fromByteArray(ch.takeRight(8)))
      .toMap
  }

  def calculateNewBalance(bxsToInsert: Seq[EncryBaseBox], bxsToRemove: Seq[EncryBaseBox]): Array[Byte] = {
    val balancesToInsert: Map[Array[Byte], Long] = BalanceCalculator.balanceSheet(bxsToInsert)
      .map(elt => elt._1 -> elt._2)
    val balancesToRemove: Map[Array[Byte], Long] = BalanceCalculator.balanceSheet(bxsToRemove)
      .map(elt => elt._1 -> elt._2)
    val oldBalances: Map[Array[Byte], Long] = getBalances
      .map(elt => elt._1 -> elt._2)
    val newBalances: Map[Array[Byte], Long] = (oldBalances.toSeq ++ balancesToInsert.toSeq)
      .groupBy(_._1)
      .foldLeft(Map.empty[Array[Byte], Long]) { case (balanceMap, tokenInfo) =>
        balanceMap.updated(tokenInfo._1, tokenInfo._2.foldLeft(0L)((tokenSum, token) => tokenSum + token._2))
      }
      .map(tokenInfo => tokenInfo._1 -> (tokenInfo._2 - balancesToRemove.filter(_._1 == tokenInfo._1).values.sum))
    newBalances.foldLeft(Array.empty[Byte]) { case (acc, (id, balance)) =>
      acc ++ id ++ Longs.toByteArray(balance)
    }
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
      Seq(WalletDiff(toDelete, toAdd, balances))
    case _ => Seq.empty
  }
}
