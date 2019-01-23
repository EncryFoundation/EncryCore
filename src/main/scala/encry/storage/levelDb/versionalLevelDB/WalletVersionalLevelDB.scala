package encry.storage.levelDb.versionalLevelDB

import com.google.common.primitives.Longs
import encry.modifiers.{EncryPersistentModifier, NodeViewModifier}
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.Box.Amount
import encry.utils.{BalanceCalculator, BoxFilter}
import encry.utils.CoreTaggedTypes.ModifierId
import org.encryfoundation.common.Algos
import org.iq80.leveldb.{DB, ReadOptions}
import cats.syntax.semigroup._
import cats.instances.all._
import encry.modifiers.state.StateModifierSerializer
import encry.modifiers.state.box.EncryBaseBox
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.utils.TaggedTypes.ADKey

import scala.util.Success

case class WalletVersionalLevelDB(override val db: DB) extends VersionalLevelDB[WalletDiff] {

  import WalletVersionalLevelDB._

  def id: ModifierId = versionsList.lastOption.map(_.modifierId).getOrElse(ModifierId @@ Array.fill(32)(0: Byte))

  override def add(modifier: NodeViewModifier): Unit = {
    if (versionsList.size + 1 > maxRollbackDepth) versionsList = versionsList.tail
    val diffs = WalletVersionalLevelDB.getDiffs(modifier)
    applyDiff(diffs.tail.foldLeft(diffs.head)(_ ++ _))
    val newModifiersTree = Version[WalletDiff](modifier.id, diffs)
    versionsList = versionsList :+ newModifiersTree
  }

  def getAllBoxes: Seq[EncryBaseBox] =
    getAll
      .map { case (key, bytes) => StateModifierSerializer.parseBytes(bytes, key.head) }
      .collect {
        case Success(box) => box
      }

  override def getAll: Seq[(Array[Byte], Array[Byte])] = {

    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    var elementsBuffer: Seq[(Array[Byte], Array[Byte])] = Seq.empty
    val iterator = db.iterator(readOptions)
    iterator.seekToFirst()
    while (iterator.hasNext) {
      val nextElem = iterator.next()
      if (nextElem.getKey sameElements WalletVersionalLevelDB.BalancesKey) elementsBuffer
      else elementsBuffer = elementsBuffer :+ (nextElem.getKey -> nextElem.getValue)
    }
    readOptions.snapshot().close()
    elementsBuffer
  }

  def getBoxes(qty: Int): Seq[EncryBaseBox] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    var elementsBuffer: Seq[(Array[Byte], Array[Byte])] = Seq.empty
    val iterator = db.iterator(readOptions)
    iterator.seekToFirst()
    while (iterator.hasNext && elementsBuffer.size < qty) {
      val nextElem = iterator.next()
      if (nextElem.getKey sameElements WalletVersionalLevelDB.BalancesKey) elementsBuffer
      else elementsBuffer = elementsBuffer :+ (nextElem.getKey -> nextElem.getValue)
    }
    readOptions.snapshot().close()
    elementsBuffer.map { case (key, bytes) => StateModifierSerializer.parseBytes(bytes, key.head) }
      .collect {
        case Success(box) => box
      }
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
      (toAddToBalance |+| toRemoveFromBalance).map { case (tokenId, value) => Algos.encode(tokenId) -> value }
    }
    val diff = WalletDiff(spentBxs.map(_.id), bxsToInsert, newBalances)
    val treeNode = Version[WalletDiff](modifierId, Seq(diff))
    if (versionsList.size + 1 > maxRollbackDepth) versionsList = versionsList.tail
    versionsList = versionsList :+ treeNode
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
    batch.close()
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

object WalletVersionalLevelDB {

  val BalancesKey: Array[Byte] = Algos.hash("balances")

  def apply(db: DB): WalletVersionalLevelDB = {
    val tree = new WalletVersionalLevelDB(db)
    if (tree.db.get(BalancesKey) == null) db.put(BalancesKey, Array.emptyByteArray)
    tree
  }

  def getDiffs(modifier: NodeViewModifier): Seq[WalletDiff] = modifier match {
    case block: Block => block.payload.transactions.flatMap(getDiffs)
    case tx: Transaction =>
      val toDelete = tx.inputs.map(_.boxId)
      val toAdd = tx.newBoxes.toList
      val balances = BalanceCalculator.balanceSheet(toAdd)
      Seq(WalletDiff(toDelete, toAdd, balances.map { case (tokenId, value) => Algos.encode(tokenId) -> value }))
    case _ => Seq.empty
  }
}
