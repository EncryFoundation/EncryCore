package encry.view.wallet

import com.google.common.primitives.Longs
import encry.modifiers.state.StateModifierDeserializer
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box._
import encry.storage.EncryStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.crypto.PublicKey25519
import org.encryfoundation.common.utils.TaggedTypes.ADKey

case class WalletStorage(store: Store, publicKeys: Set[PublicKey25519]) extends EncryStorage {

  import WalletStorage._

  def getBoxById(id: ADKey): Option[EncryBaseBox] = store.get(keyByBoxId(id))
    .flatMap(d => StateModifierDeserializer.parseBytes(d.data, id.head).toOption)

  def allBoxes: Seq[EncryBaseBox] = store.getAll
    .filter(_._2 == balancesKey)
    .foldLeft(Seq[EncryBaseBox]()) { case (acc, id) =>
      getBoxById(ADKey @@ id._1.data).map(bx => acc :+ bx).getOrElse(acc)
    }

  def containsBox(id: ADKey): Boolean = getBoxById(id).isDefined

  def getTokenBalanceById(id: TokenId): Option[Amount] = getBalances
    .find(_._1 sameElements id)
    .map(_._2)

  def getBalances: Map[TokenId, Amount] = store.get(balancesKey)
    .map {
      _.data
        .sliding(40, 40)
        .map(ch => ch.take(32) -> Longs.fromByteArray(ch.takeRight(8)))
    }
    .map(_.toMap)
    .getOrElse(Map.empty)
}

object WalletStorage {

  val balancesKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash("balances"))

  def keyByBoxId(id: ADKey): ByteArrayWrapper = ByteArrayWrapper(id)
}
