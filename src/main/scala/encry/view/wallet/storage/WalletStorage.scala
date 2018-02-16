package encry.view.wallet.storage

import encry.modifiers.mempool.{PaymentTransaction, PaymentTransactionSerializer}
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

  def updateWithReplacement(id: ModifierId,
                            idsToReplace: Seq[ByteArrayWrapper],
                            toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit =
    updateWithReplacement(ByteArrayWrapper(id), idsToReplace, toInsert)

  def nonVersionedInsert(toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(Random.nextLong(), Seq(), toInsert)
  }

  def packBoxIds(ids: Seq[ADKey]): ByteArrayWrapper =
    new ByteArrayWrapper(ids.foldLeft(Array[Byte]())(_ ++ _))

  def packTransactionIds(ids: Seq[ModifierId]): ByteArrayWrapper =
    new ByteArrayWrapper(ids.foldLeft(Array[Byte]())(_ ++ _))

  def getBoxIds: Seq[ADKey] =
    parseComplexValue(boxIdsKey, 32).map(ADKey @@ _).getOrElse(Seq())

  def getTransactionIds: Seq[ModifierId] =
    parseComplexValue(transactionIdsKey, 32).map(ModifierId @@ _).getOrElse(Seq())

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
}

object WalletStorage {

  val boxIdsKey = ByteArrayWrapper(Algos.hash("listOfBoxesKeys"))

  val transactionIdsKey = ByteArrayWrapper(Algos.hash("listOfTransactions"))

  val balanceKey = ByteArrayWrapper(Algos.hash("balance"))

  def boxKeyById(id: ADKey): ByteArrayWrapper = ByteArrayWrapper(id)

  def txKeyById(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(id)
}
