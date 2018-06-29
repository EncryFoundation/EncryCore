package encry.view.wallet.storage

import com.google.common.primitives.Longs
import encry.crypto.PublicKey25519
import encry.modifiers.state.StateModifierDeserializer
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box._
import encry.settings.Algos
import encry.storage.EncryBaseStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.crypto.authds.ADKey

case class WalletStorage(store: Store, publicKeys: Set[PublicKey25519]) extends EncryBaseStorage {

  import WalletStorage._

  def getBoxById(id: ADKey): Option[EncryBaseBox] = store.get(keyByBoxId(id))
    .flatMap(d => StateModifierDeserializer.parseBytes(d.data, id.head).toOption)

  def allBoxes: Seq[EncryBaseBox] = store.getAll
    .filter(dataFromStore => getTokensId.contains(dataFromStore._1.data) || dataFromStore._1.equals(tokensIdsKey))
    .foldLeft(Seq[EncryBaseBox]()) { case (acc, id) =>
      getBoxById(ADKey @@ id._1.data).map(bx => acc :+ bx).getOrElse(acc)
    }

  def containsBox(id: ADKey): Boolean = getBoxById(id).isDefined

  def getTokenBalanceById(id: ADKey): Option[Amount] = store.get(keyByTokenId(id)).map(v => Longs.fromByteArray(v.data))

  def getTokensId: Seq[ADKey] = readComplexValue(tokensIdsKey, 32).map(ADKey @@ _).getOrElse(Seq())
}

object WalletStorage {

  val tokensIdsKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash("tokens_id"))

  def keyByBoxId(id: ADKey): ByteArrayWrapper = ByteArrayWrapper(id)

  def keyByTokenId(id: ADKey): ByteArrayWrapper = ByteArrayWrapper(id)
}
