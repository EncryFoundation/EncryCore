package encry.view.wallet

import com.google.common.primitives.Longs
import encry.modifiers.state.StateModifierDeserializer
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box._
import encry.stats.StatsSender.{GetAllTiming, WorkedTime}
import encry.storage.EncryStorage
import encry.EncryApp.{settings, system}
import io.iohk.iodb.{ByteArrayWrapper, Store}
import io.iohk.iodb.Store.{K, V}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.crypto.PublicKey25519
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import WalletStorage._

case class WalletStorage(store: Store, publicKeys: Set[PublicKey25519]) extends EncryStorage {

  def allBoxes: Seq[EncryBaseBox] = {
    val startTime: Amount = System.nanoTime()
    val storeGetAllSeq: Iterator[(K, V)] = store.getAll
    val endStoreGetAllTime: Amount = System.nanoTime() - startTime

    if (settings.influxDB.isDefined)
      system.actorSelection("user/statsSender") ! GetAllTiming(endStoreGetAllTime, storeGetAllSeq.size)

    val outputs: Vector[EncryBaseBox] = store.getAll.foldLeft(Vector[EncryBaseBox]()) {
      case (acc, pair) if pair._1 != balancesKey =>
        StateModifierDeserializer.parseBytes(pair._2.data, pair._1.data.head)
          .map(boxes => acc :+ boxes).getOrElse(acc)
      case (acc, _) => acc
    }

    if (settings.influxDB.isDefined)
      system.actorSelection("user/statsSender") ! WorkedTime(System.nanoTime() - startTime, outputs.size)

    outputs
  }

  def getBoxById(id: ADKey): Option[EncryBaseBox] = store.get(keyByBoxId(id))
    .flatMap(d => StateModifierDeserializer.parseBytes(d.data, id.head).toOption)

  def containsBox(id: ADKey): Boolean = getBoxById(id).isDefined

  def getTokenBalanceById(id: TokenId): Option[Amount] = getBalances.find(_._1 sameElements id).map(_._2)

  def getBalances: Map[TokenId, Amount] = store.get(balancesKey).map {
    _.data.sliding(40, 40).map(ch => ch.take(32) -> Longs.fromByteArray(ch.takeRight(8))) }
    .map(_.toMap).getOrElse(Map.empty)
}

object WalletStorage {

  val balancesKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash("balances"))

  def keyByBoxId(id: ADKey): ByteArrayWrapper = ByteArrayWrapper(id)
}