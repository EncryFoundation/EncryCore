package encry.view.wallet.storage

import com.google.common.primitives.Longs
import encry.modifiers.mempool.{CoinbaseTransaction, EncryBaseTransaction, PaymentTransaction, PaymentTransactionSerializer}
import encry.modifiers.state.box.{AssetBox, AssetBoxSerializer}
import encry.settings.Algos
import encry.view.EncryBaseStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32

import scala.util.Try

class WalletStorage(val db: Store, val publicKeys: Set[PublicKey25519Proposition]) extends EncryBaseStorage {

  import WalletStorage._

  /**
    * Get keys of unspent boxes
    * @return seq of ADKeys
    */
  def getBoxIds: Seq[ADKey] ={
    val boxIdsRaw = db.get(boxIdsKey).map(v =>v.data).getOrElse(Array[Byte]())
    (0 until (boxIdsRaw.length/32)).foldLeft(Seq[ADKey]())( (seq,i) =>
      seq :+ ADKey @@ boxIdsRaw.slice(32*i,32*i+32)
    )
  }

  /**
    * Get keys of all transactions
    * @return seq of Digest32
    */
  def getTransactionIds: Seq[Digest32] =
    db.get(boxIdsKey).map { v =>
      v.data.sliding(1, 32).foldLeft(Seq[Digest32]())((seq, key) => seq :+ Digest32 @@ key)
    }.getOrElse(Seq())

  /**
    * Update value of key "listOfBoxesKeys"
    * @param newList - new value
    */
  def updateADKeysList(newList: Array[Byte]): Unit = {
    //delete previous value
    db.update(
      new ByteArrayWrapper(Algos.hash(newList ++ Longs.toByteArray(System.currentTimeMillis()))), Seq(boxIdsKey), Seq()
    )
    //put new value
    db.update(
      new ByteArrayWrapper(Algos.hash(Algos.hash(newList ++ Longs.toByteArray(System.currentTimeMillis())))),
      Seq(),
      Seq((boxIdsKey, new ByteArrayWrapper(newList)))
    )
  }

  /**
    * Update value of key "listOfTransactions"
    * @param newList - new value
    */
  def updateTrxList(newList: Array[Byte]): Unit ={
    //delete previous value
    db.update(
      new ByteArrayWrapper(Algos.hash(newList ++ Longs.toByteArray(System.currentTimeMillis()))), Seq(transactionIdsKey), Seq()
    )
    //put new value
    db.update(
      new ByteArrayWrapper(Algos.hash(Algos.hash(newList ++ Longs.toByteArray(System.currentTimeMillis())))),
      Seq(),
      Seq((transactionIdsKey, new ByteArrayWrapper(newList)))
    )
  }

  def updateBalance(newBalance: Long): Unit = {
    //delete previous value
    db.update(
      new ByteArrayWrapper(Algos.hash(Longs.toByteArray(newBalance + System.currentTimeMillis()))),
      Seq(balanceKey),
      Seq()
    )
    //put new value
    db.update(
      new ByteArrayWrapper(Algos.hash(Algos.hash(Longs.toByteArray(newBalance + System.currentTimeMillis())))),
      Seq(),
      Seq((balanceKey, new ByteArrayWrapper(Longs.toByteArray(newBalance))))
    )
  }

  /**
    * Put box to store
    * @param box
    */
  def putBox(box: AssetBox): Unit = {
    if(getBoxById(box.id).isFailure) {
      db.update(
        new ByteArrayWrapper(Algos.hash(box.id ++ Longs.toByteArray(System.currentTimeMillis()))),
        Seq(),
        Seq((new ByteArrayWrapper(Algos.hash(box.id)), new ByteArrayWrapper(AssetBoxSerializer.toBytes(box))))
      )
    } else throw new Error("Box with this id is already contains in db")
  }

  def putTransaction(tx: EncryBaseTransaction): Unit = {
    if(getTransactionById(tx.txHash).isFailure){
      val txsRawBytes = db.get(transactionIdsKey).map(_.data).getOrElse(Array[Byte]())
      if (publicKeys.contains(tx.proposition) && !tx.isInstanceOf[CoinbaseTransaction]) {
        updateTrxList(txsRawBytes ++ tx.txHash)
        db.update(
          new ByteArrayWrapper(tx.txHash),
          Seq(),
          Seq((new ByteArrayWrapper(tx.txHash), new ByteArrayWrapper(tx.bytes)))
        )
        deleteBoxesById(tx.useBoxes)
      } else {
        updateTrxList(txsRawBytes ++ tx.txHash)
        db.update(
          new ByteArrayWrapper(tx.txHash),
          Seq(),
          Seq((new ByteArrayWrapper(tx.txHash), new ByteArrayWrapper(tx.bytes)))
        )
        putBoxes(tx.newBoxes.filter(box => box.isInstanceOf[AssetBox] &&
          publicKeys.map(a => a.address).contains(box.asInstanceOf[AssetBox].proposition.address))
          .map(_.asInstanceOf[AssetBox]).toSeq)
      }
      refreshBalance()
    } else {
      throw new Error("Tx with this txHash is already contains in db")
    }
  }

  /**
    * Put seq of boxes to store
    * @param boxList
    */
  def putBoxes(boxList: Seq[AssetBox]): Unit = {
    if(getBoxIds.isEmpty) {
      updateADKeysList(boxList.foldLeft(Array[Byte]())(_ ++ _.id))
    }
    else
      updateADKeysList(
        db.get(boxIdsKey).map(_.data)
          .getOrElse(Array[Byte]()) ++ boxList.foldLeft(Array[Byte]())(_ ++ _.id))
    boxList.foreach(putBox)
  }

  /**
    * Delete box from store by Id
    * @param id
    */
  def deleteBoxById(id: ADKey): Unit = {
    updateADKeysList(
      db.get(boxIdsKey).map(_.data)
        .getOrElse(Array[Byte]()).sliding(32).foldLeft(Array[Byte]()) { case (buff, key) =>
      if (!(key sameElements id)) buff ++ key else buff
    })
    db.update(System.currentTimeMillis(), Seq(ByteArrayWrapper(id)), Seq())
  }

  def deleteBoxesById(ids: Seq[ADKey]): Unit = {
    val newList = getBoxIds.foldLeft(Array[Byte]()) {
      case (buff, id) => if (ids.forall(!_.sameElements(id))) buff ++ id else buff }

    updateADKeysList(newList)
  }

  /**
    * Return instance of box
    * @param id - box id
    * @return
    */
  def getBoxById(id: ADKey): Try[AssetBox] = Try {
    AssetBoxSerializer.parseBytes(db.get(new ByteArrayWrapper(Algos.hash(id))).get.data).get
  }

  def getAllBoxes: Seq[AssetBox] =
    getBoxIds.foldLeft(Seq[AssetBox]()) { case (buff, id) =>
      val bx = getBoxById(id)
      if (bx.isSuccess) buff :+ bx.get else buff
    }

  /**
    * Return instance of transaction
    * @param txHash - hash of transaction
    * @return
    */
  def getTransactionById(txHash: Digest32): Try[PaymentTransaction] = Try {
    PaymentTransactionSerializer.parseBytes(db.get(new ByteArrayWrapper(txHash)).get.data).get
  }

  def refreshBalance(): Unit ={
    updateBalance(getAllBoxes.foldLeft(0L)(_ + _.amount))
  }

}

object WalletStorage {

  val boxIdsKey = ByteArrayWrapper(Algos.hash("listOfBoxesKeys"))

  val transactionIdsKey = ByteArrayWrapper(Algos.hash("listOfTransactions"))

  val balanceKey = ByteArrayWrapper(Algos.hash("balance"))
}