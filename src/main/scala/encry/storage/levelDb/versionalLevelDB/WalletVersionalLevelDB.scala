package encry.storage.levelDb.versionalLevelDB

import BoxesProto.BoxProtoMessage.Box.DataBox
import cats.instances.all._
import cats.syntax.semigroup._
import com.google.common.primitives.Longs
import com.typesafe.scalalogging.StrictLogging
import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion._
import encry.utils.{BalanceCalculator, ByteStr}
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.EncryBaseBox
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, ModifierId}
import org.iq80.leveldb.DB
import scorex.crypto.hash.Digest32

import scala.util.Success

case class WalletVersionalLevelDB(db: DB, settings: LevelDBSettings) extends StrictLogging with AutoCloseable {

  import WalletVersionalLevelDBCompanion._

  val levelDb: VersionalLevelDB = VersionalLevelDB(db, settings)

  //todo: optimize this
  def getAllBoxes(maxQty: Int = -1): Seq[EncryBaseBox] = levelDb.getAll(maxQty)
    .filterNot(_._1 sameElements BALANCE_KEY)
    .map { case (key, bytes) => StateModifierSerializer.parseBytes(bytes, key.head) }
    .collect { case Success(box) => box }

  def getBoxById(id: ADKey): Option[EncryBaseBox] = levelDb.get(VersionalLevelDbKey @@ id.untag(ADKey))
    .flatMap(wrappedBx => StateModifierSerializer.parseBytes(wrappedBx, id.head).toOption)

  def getTokenBalanceById(id: TokenId): Option[Amount] = getBalances
    .find(_._1._2 == Algos.encode(id))
    .map(_._2)

  def containsBox(id: ADKey): Boolean = getBoxById(id).isDefined

  def rollback(modId: ModifierId): Unit = levelDb.rollbackTo(LevelDBVersion @@ modId.untag(ModifierId))

  def updateWallet(modifierId: ModifierId, newBxs: Seq[EncryBaseBox], spentBxs: Seq[EncryBaseBox],
                   intrinsicTokenId: ADKey): Unit = {
    println("here wallet = " + newBxs)
    println("here wallet1 = " + spentBxs)
    val bxsToInsert: Seq[EncryBaseBox] = newBxs.filter(bx => !spentBxs.contains(bx))
    val newBalances: Map[(String, String), Amount] = {
      val toRemoveFromBalance = BalanceCalculator.balanceSheet(spentBxs, intrinsicTokenId)
        .map { case ((hash, key), value) => (hash, ByteStr(key)) -> value * -1 }
      val toAddToBalance = BalanceCalculator.balanceSheet(newBxs, intrinsicTokenId)
        .map { case ((hash, key), value) => (hash, ByteStr(key)) -> value }
      val prevBalance = getBalances.map { case ((hash, id), value) => (hash, ByteStr(Algos.decode(id).get)) -> value }
      (toAddToBalance |+| toRemoveFromBalance |+| prevBalance).map { case ((hash, tokenId), value) => (hash, tokenId.toString) -> value }
    }
    val newBalanceKeyValue = BALANCE_KEY -> VersionalLevelDbValue @@
      newBalances.foldLeft(Array.emptyByteArray) { case (acc, ((hash, tokenId), balance)) =>
        acc ++ Algos.decode(hash).get ++ Algos.decode(tokenId).get ++ Longs.toByteArray(balance)
      }
    val newDataBoxes: Seq[DataBox] = newBxs.map(x => x.typeId match {
      case 4 => x.asInstanceOf[DataBox].
    })
    val bbb =
    levelDb.insert(LevelDbDiff(LevelDBVersion @@ modifierId.untag(ModifierId),
      newBalanceKeyValue :: bxsToInsert.map(bx => (VersionalLevelDbKey @@ bx.id.untag(ADKey),
        VersionalLevelDbValue @@ bx.bytes)).toList,
      spentBxs.map(elem => VersionalLevelDbKey @@ elem.id.untag(ADKey)))
    )
//    levelDb.insert(LevelDbDiff(LevelDBVersion @@ modifierId.untag(ModifierId),
//
//    ))
  }

  def getBalances: Map[(String, String), Amount] =
    levelDb.get(BALANCE_KEY)
      .map(_.sliding(72, 72)
        .map(ch => (Algos.encode(ch.take(32)), Algos.encode(ch.slice(32, 64))) -> Longs.fromByteArray(ch.takeRight(8)))
        .toMap).getOrElse(Map.empty)

  override def close(): Unit = levelDb.close()
}

object WalletVersionalLevelDBCompanion extends StrictLogging {

  val BALANCE_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ Algos.hash("BALANCE_KEY").untag(Digest32)

  val DATA_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ Algos.hash("DATA_KEY").untag(Digest32)

  val INIT_MAP: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map(
    BALANCE_KEY -> VersionalLevelDbValue @@ Array.emptyByteArray
  )

  def apply(levelDb: DB, settings: LevelDBSettings): WalletVersionalLevelDB = {
    val db = WalletVersionalLevelDB(levelDb, settings)
    db.levelDb.recoverOrInit(INIT_MAP)
    db
  }
}
