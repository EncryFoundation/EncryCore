package encry.storage.levelDb.versionalLevelDB

import cats.instances.all._
import cats.syntax.semigroup._
import com.google.common.primitives.Longs
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.state.StateModifierSerializer
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.EncryBaseBox
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.settings.Constants
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion._
import encry.utils.{BalanceCalculator, ByteStr}
import encry.utils.CoreTaggedTypes.ModifierId
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.iq80.leveldb.DB
import scorex.crypto.hash.Digest32

import scala.util.Success

case class WalletVersionalLevelDB(db: DB) extends StrictLogging {

  import WalletVersionalLevelDBCompanion._

  val levelDb: VersionalLevelDB = VersionalLevelDB(db)

  def getAllBoxes: Seq[EncryBaseBox] = levelDb.getAll
      .map { case (key, bytes) => StateModifierSerializer.parseBytes(bytes.data, key.data.head) }
      .collect {
        case Success(box) => box
      }

  def getBoxById(id: ADKey): Option[EncryBaseBox] = {
    levelDb.get(VersionalLevelDbKey @@ new ByteArrayWrapper(id))
      .flatMap(wrappedBx => StateModifierSerializer.parseBytes(wrappedBx.data, id.head).toOption)
  }

  def getTokenBalanceById(id: TokenId): Option[Amount] = getBalances
    .find(_._1 sameElements Algos.encode(id))
    .map(_._2)

  def containsBox(id: ADKey): Boolean = getBoxById(id).isDefined

  def rollback(modId: ModifierId): Unit = levelDb.rollbackTo(LevelDBVersion @@ new ByteArrayWrapper(modId.untag(ModifierId)))

  def updateWallet(modifierId: ModifierId, newBxs: Seq[EncryBaseBox], spentBxs: Seq[EncryBaseBox]): Unit = {
    val bxsToInsert: Seq[EncryBaseBox] = newBxs.filter(bx => !spentBxs.contains(bx))
    val newBalances: Map[String, Amount] = {
      val toRemoveFromBalance = BalanceCalculator.balanceSheet(spentBxs).map{case (key, value) => ByteStr(key) -> value * -1}
      val toAddToBalance = BalanceCalculator.balanceSheet(newBxs).map{case (key, value) => ByteStr(key) -> value}
      val prevBalance = getBalances.map{case (id, value) => ByteStr(Algos.decode(id).get) -> value}
      (toAddToBalance |+| toRemoveFromBalance |+| prevBalance).map { case (tokenId, value) => tokenId.toString -> value }
    }
    val newBalanceKeyValue = BALANCE_KEY -> VersionalLevelDbValue @@
      new ByteArrayWrapper(newBalances.foldLeft(Array.emptyByteArray) { case (acc, (id, balance)) =>
        acc ++ Algos.decode(id).get ++ Longs.toByteArray(balance)
       })
    levelDb.insert(LevelDbElem(LevelDBVersion @@ new ByteArrayWrapper(modifierId.untag(ModifierId)),
      newBalanceKeyValue :: bxsToInsert.map(bx => (VersionalLevelDbKey @@ new ByteArrayWrapper(bx.id.untag(ADKey)),
        VersionalLevelDbValue @@ new ByteArrayWrapper(bx.bytes))).toList,
      spentBxs.map(elem => VersionalLevelDbKey @@ new ByteArrayWrapper(elem.id.untag(ADKey))))
    )
  }

  def getBalances: Map[String, Amount] =
    levelDb.get(BALANCE_KEY)
      .map(_.data.sliding(40, 40)
        .map(ch => Algos.encode(ch.take(32)) -> Longs.fromByteArray(ch.takeRight(8)))
        .toMap).getOrElse(Map.empty)
}

object WalletVersionalLevelDBCompanion extends StrictLogging {

  val BALANCE_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ new ByteArrayWrapper(Algos.hash("BALANCE_KEY").untag(Digest32))

  val INIT_MAP: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map(
    BALANCE_KEY -> VersionalLevelDbValue @@ new ByteArrayWrapper(Array.emptyByteArray)
  )

  def apply(levelDb: DB): WalletVersionalLevelDB = {
    val db = WalletVersionalLevelDB(levelDb)
    db.levelDb.recoverOrInit(INIT_MAP)
    db
  }
}
