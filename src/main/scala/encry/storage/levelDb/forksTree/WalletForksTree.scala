package encry.storage.levelDb.forksTree
import com.google.common.primitives.Longs
import encry.modifiers.{EncryPersistentModifier, NodeViewModifier}
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.Box.Amount
import encry.utils.{BalanceCalculator, BoxFilter}
import encry.utils.CoreTaggedTypes.ModifierId
import org.encryfoundation.common.Algos
import org.iq80.leveldb.DB
import cats.syntax.semigroup._
import cats.instances.all._
import encry.modifiers.state.StateModifierSerializer
import encry.modifiers.state.box.EncryBaseBox
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.utils.TaggedTypes.ADKey

case class WalletForksTree(override val db: DB) extends ForksTree[WalletDiff] {

  import WalletForksTree._

  def id: ModifierId = modifiersTree.last.modifierId

  override def add(modifier: NodeViewModifier): Unit = {
    val diffs = WalletForksTree.getDiffs(modifier)
    applyDiff(diffs.tail.foldLeft(diffs.head)(_ ++ _))
    val newModifiersTree = ForksTreeNode[WalletDiff](modifier.id, diffs)
    modifiersTree = modifiersTree :+ newModifiersTree
  }

  def getBoxById(id: ADKey): Option[EncryBaseBox] = StateModifierSerializer.parseBytes(db.get(id), id.head).toOption

  def getTokenBalanceById(id: TokenId): Option[Amount] = getBalances
    .find(_._1 sameElements Algos.encode(id))
    .map(_._2)

  def containsBox(id: ADKey): Boolean = getBoxById(id).isDefined

  def updateWallet(modifierId: ModifierId, newBxs: Seq[EncryBaseBox], spentBxs: Seq[EncryBaseBox]): Unit = {
    val bxsToInsert: Seq[EncryBaseBox] = newBxs.filter(bx => !spentBxs.contains(bx))
    val newBalances: Map[String, Amount] = {
      val toRemoveFromBalance = BalanceCalculator.balanceSheet(spentBxs).mapValues(_ * -1)
      val toAddToBalance = BalanceCalculator.balanceSheet(newBxs)
      (toAddToBalance |+| toRemoveFromBalance).map{case (tokenId, value) => Algos.encode(tokenId) -> value}
    }
    val diff = WalletDiff(spentBxs.map(_.id), bxsToInsert, newBalances)
    val treeNode = ForksTreeNode[WalletDiff](modifierId, Seq(diff))
    modifiersTree = modifiersTree :+ treeNode
    applyDiff(diff)
  }

  def applyDiff(diff: WalletDiff): Unit = {
    val batch = db.createWriteBatch()
    diff.boxesToRemove.foreach(key => batch.delete(key))
    diff.boxesToAdd.foreach(box => batch.put(box.id, box.bytes))
    val previousBalances: Map[String, Amount] = getBalances
    val newBalances: Map[String, Amount] = diff.balanceChanges |+| previousBalances
    batch.put(BalancesKey, newBalances.foldLeft(Array.empty[Byte]) { case (acc, (id, balance)) =>
      acc ++ Algos.decode(id).get ++ Longs.toByteArray(balance)
    })
    db.write(batch)
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
    if (tree.db.get(BalancesKey) == null) db.put(BalancesKey, Array.emptyByteArray)
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
