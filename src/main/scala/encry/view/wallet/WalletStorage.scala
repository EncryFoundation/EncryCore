package encry.view.wallet

import com.google.common.primitives.Longs
import encry.modifiers.state.StateModifierDeserializer
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box._
import encry.stats.StatsSender.{GetAllTiming, WorkedTime}
import encry.storage.EncryStorage
import encry.EncryApp.system
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.crypto.PublicKey25519
import org.encryfoundation.common.utils.TaggedTypes.ADKey

case class WalletStorage(store: Store, publicKeys: Set[PublicKey25519]) extends EncryStorage {

  import WalletStorage._

  def getBoxById(id: ADKey): Option[EncryBaseBox] = store.get(keyByBoxId(id))
    .flatMap(d => StateModifierDeserializer.parseBytes(d.data, id.head).toOption)

  def allBoxes: Seq[EncryBaseBox] = {
    val a = System.currentTimeMillis()
    val a1 = store.getAll
    val c1 = System.currentTimeMillis() - a
    system.actorSelection("user/statsSender") ! GetAllTiming(c1, a1.size)


    val b: Seq[EncryBaseBox] = store.getAll.filter { x => x._2 != balancesKey }.foldLeft(Seq[EncryBaseBox]()) {
      case (acc, id) => getBoxById(ADKey @@ id._1.data).map { bx => acc :+ bx }.getOrElse(acc)
    }


    val c = System.currentTimeMillis() - a
    println(c + " time")
    println(b.size + " size of func")
    system.actorSelection("user/statsSender") ! WorkedTime(c, b.size)
    b
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
