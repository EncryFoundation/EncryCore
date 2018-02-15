package encry.view.wallet.storage

import com.google.common.primitives.Longs
import encry.modifiers.mempool.{CoinbaseTransaction, EncryBaseTransaction, PaymentTransaction, PaymentTransactionSerializer}
import encry.modifiers.state.box.{AssetBox, AssetBoxSerializer}
import encry.settings.Algos
import encry.view.EncryBaseStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.authds.ADKey

import scala.util.{Random, Try}

class WalletStorage(val db: Store, val publicKeys: Set[PublicKey25519Proposition])
  extends EncryBaseStorage {

  import WalletStorage._

  def nonVersionedUpdateWithReplacement(idsToRemove: Seq[ByteArrayWrapper],
                                        toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(Random.nextLong(), idsToRemove, Seq())
    db.update(Random.nextLong(), Seq(), toInsert)
  }

  def nonVersionedUpdate(idsToRemove: Seq[ByteArrayWrapper],
                         toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(Random.nextLong(), idsToRemove, toInsert)
  }

  def nonVersionedInsert(toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(Random.nextLong(), Seq(), toInsert)
  }

  def packBoxIds(ids: Seq[ADKey]): ByteArrayWrapper =
    new ByteArrayWrapper(ids.foldLeft(Array[Byte]())(_ ++ _))

  def getBoxIds: Seq[ADKey] =
    parseComplexValue(boxIdsKey, 32).map(ADKey @@ _).getOrElse(Seq())

  def getTransactionIds: Seq[ModifierId] =
    parseComplexValue(transactionIdsKey, 32).map(ModifierId @@ _).getOrElse(Seq())

  def updateBalance(newBalance: Long): Unit =
    nonVersionedUpdateWithReplacement(Seq(balanceKey),
      Seq(balanceKey -> ByteArrayWrapper(Longs.toByteArray(newBalance))))

  def putTransaction(tx: EncryBaseTransaction): Unit = {
    if(getTransactionById(tx.id).isEmpty){
      val txIdsRaw = get(transactionIdsKey).getOrElse(Array[Byte]())
      nonVersionedUpdateWithReplacement(Seq(transactionIdsKey),
        Seq(transactionIdsKey -> ByteArrayWrapper(txIdsRaw ++ tx.id)))
      nonVersionedInsert(Seq(txKeyById(tx.id) -> ByteArrayWrapper(tx.bytes)))
      if (!tx.isInstanceOf[CoinbaseTransaction]) deleteBoxesById(tx.useBoxes)
      putBoxes(tx.newBoxes.filter(box => box.isInstanceOf[AssetBox] &&
        publicKeys.map(a => a.address).contains(box.asInstanceOf[AssetBox].proposition.address))
        .map(_.asInstanceOf[AssetBox]).toSeq)
      refreshBalance()
    }
  }

  def putBox(box: AssetBox): Unit = {
    if(getBoxById(box.id).isEmpty) {
      nonVersionedInsert(Seq(boxKeyById(box.id) -> ByteArrayWrapper(AssetBoxSerializer.toBytes(box))))
    }
  }

  def putBoxes(boxes: Seq[AssetBox]): Unit = {
    val boxIdsOpt = db.get(boxIdsKey)
    if (boxIdsOpt.isEmpty) {
      nonVersionedInsert(Seq(boxIdsKey -> packBoxIds(boxes.map(_.id))))
    } else {
      val oldValue = boxIdsOpt.map(_.data).getOrElse(Array.empty[Byte])
      val newValue = oldValue ++ boxes.foldLeft(Array[Byte]())(_ ++ _.id)
      nonVersionedUpdateWithReplacement(Seq(boxIdsKey), Seq(boxIdsKey -> ByteArrayWrapper(newValue)))
    }
    boxes.foreach(putBox)
  }

  def deleteBoxById(id: ADKey): Unit = {
    nonVersionedUpdateWithReplacement(Seq(boxIdsKey),
      Seq(boxIdsKey -> packBoxIds(getBoxIds.filterNot(_ sameElements id))))
  }

  def deleteBoxesById(ids: Seq[ADKey]): Unit = {
    nonVersionedUpdateWithReplacement(Seq(boxIdsKey),
      Seq(boxIdsKey -> packBoxIds(getBoxIds.foldLeft(Seq[ADKey]())((acc, id) =>
        if (!ids.exists(_ sameElements id)) acc :+ id else acc))))
  }

  def getBoxById(id: ADKey): Option[AssetBox] = Try {
      AssetBoxSerializer.parseBytes(db.get(boxKeyById(id)).get.data).get
    }.toOption

  def getAllBoxes: Seq[AssetBox] =
    getBoxIds.foldLeft(Seq[AssetBox]()) { case (buff, id) =>
      val bx = getBoxById(id)
      if (bx.isDefined) buff :+ bx.get else buff
    }

  def getTransactionById(id: ModifierId): Option[PaymentTransaction] = Try {
    PaymentTransactionSerializer.parseBytes(db.get(ByteArrayWrapper(id)).get.data).get
  }.toOption

  def refreshBalance(): Unit ={
    updateBalance(getAllBoxes.foldLeft(0L)(_ + _.amount))
  }
}

object WalletStorage {

  val boxIdsKey = ByteArrayWrapper(Algos.hash("listOfBoxesKeys"))

  val transactionIdsKey = ByteArrayWrapper(Algos.hash("listOfTransactions"))

  val balanceKey = ByteArrayWrapper(Algos.hash("balance"))

  def boxKeyById(id: ADKey): ByteArrayWrapper = ByteArrayWrapper(id)

  def txKeyById(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(id)
}
