package encry.view.wallet.storage

import encry.crypto.PublicKey25519
import encry.modifiers.mempool.{EncryTransaction, EncryTransactionSerializer}
import encry.modifiers.state.StateModifierDeserializer
import encry.modifiers.state.box._
import encry.settings.Algos
import encry.view.EncryBaseStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.crypto.authds.ADKey

import scala.util.Try

class WalletStorage(val store: Store, val publicKeys: Set[PublicKey25519])
  extends EncryBaseStorage {

  import WalletStorage._

  def updateWithReplacement(id: ModifierId,
                            idsToReplace: Seq[ByteArrayWrapper],
                            toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit =
    updateWithReplacement(ByteArrayWrapper(id), idsToReplace, toInsert)

  def packBoxIds(ids: Seq[ADKey]): ByteArrayWrapper =
    ByteArrayWrapper(ids.foldLeft(Array[Byte]())(_ ++ _))

  def packTransactionIds(ids: Seq[ModifierId]): ByteArrayWrapper =
    ByteArrayWrapper(ids.foldLeft(Array[Byte]())(_ ++ _))

  def getBoxIds: Seq[ADKey] =
    parseComplexValue(boxIdsKey, 32).map(ADKey @@ _).getOrElse(Seq())

  def getTransactionIds: Seq[ModifierId] =
    parseComplexValue(transactionIdsKey, 32).map(ModifierId @@ _).getOrElse(Seq())

  def getBoxById(id: ADKey): Option[EncryBaseBox] = store.get(boxKeyById(id))
    .flatMap(d => StateModifierDeserializer.parseBytes(d.data, id.head).toOption)

  def getAllBoxes: Seq[EncryBaseBox] =
    getBoxIds.foldLeft(Seq[EncryBaseBox]()) { case (buff, id) =>
      val bx = getBoxById(id)
      if (bx.isDefined) buff :+ bx.get else buff
    }

  def getTransactionById(id: ModifierId): Option[EncryTransaction] = Try {
    EncryTransactionSerializer.parseBytes(store.get(ByteArrayWrapper(id)).get.data).get
  }.toOption
}

object WalletStorage {

  val boxIdsKey = ByteArrayWrapper(Algos.hash("listOfBoxesKeys"))

  val transactionIdsKey = ByteArrayWrapper(Algos.hash("listOfTransactions"))

  val balanceKey = ByteArrayWrapper(Algos.hash("balance"))

  def boxKeyById(id: ADKey): ByteArrayWrapper = ByteArrayWrapper(id)

  def txKeyById(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(id)
}
