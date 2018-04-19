package encry.view.wallet.storage

import encry.crypto.PublicKey25519
import encry.modifiers.mempool.{EncryTransaction, EncryTransactionSerializer}
import encry.modifiers.state.StateModifierDeserializer
import encry.modifiers.state.box._
import encry.settings.Algos
import encry.storage.EncryBaseStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.crypto.authds.ADKey

import scala.util.Try

class WalletStorage(val store: Store, val publicKeys: Set[PublicKey25519])
  extends EncryBaseStorage {

  import WalletStorage._

  def packBoxIds(ids: Seq[ADKey]): ByteArrayWrapper =
    ByteArrayWrapper(ids.foldLeft(Array[Byte]())(_ ++ _))

  def packTransactionIds(ids: Seq[ModifierId]): ByteArrayWrapper =
    ByteArrayWrapper(ids.foldLeft(Array[Byte]())(_ ++ _))

  def boxIds: Seq[ADKey] =
    readComplexValue(boxIdsKey, 32).map(ADKey @@ _).getOrElse(Seq())

  def openBoxIds: Seq[ADKey] =
    readComplexValue(openBoxesIdsKey, 32).map(ADKey @@ _).getOrElse(Seq())

  def transactionIds: Seq[ModifierId] =
    readComplexValue(transactionIdsKey, 32).map(ModifierId @@ _).getOrElse(Seq())

  def getBoxById(id: ADKey): Option[EncryBaseBox] = store.get(keyByBoxId(id))
    .flatMap(d => StateModifierDeserializer.parseBytes(d.data, id.head).toOption)

  def allBoxes: Seq[EncryBaseBox] =
    boxIds.foldLeft(Seq[EncryBaseBox]()) { case (acc, id) =>
      getBoxById(id).map(bx => acc :+ bx).getOrElse(acc)
    }

  def getTransactionById(id: ModifierId): Option[EncryTransaction] = Try {
    EncryTransactionSerializer.parseBytes(store.get(ByteArrayWrapper(id)).get.data).get
  }.toOption
}

object WalletStorage {

  val boxIdsKey = ByteArrayWrapper(Algos.hash("account_boxes"))

  val openBoxesIdsKey = ByteArrayWrapper(Algos.hash("open_boxes"))

  val transactionIdsKey = ByteArrayWrapper(Algos.hash("account_transactions"))

  val tokenBalanceKey = ByteArrayWrapper(Algos.hash("token"))

  val encryBalanceKey = ByteArrayWrapper(Algos.hash("encry_balance"))

  def keyByBoxId(id: ADKey): ByteArrayWrapper = ByteArrayWrapper(id)

  def txKeyById(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(id)
}
