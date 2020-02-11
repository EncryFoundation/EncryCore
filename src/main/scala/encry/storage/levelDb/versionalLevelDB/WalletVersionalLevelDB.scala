package encry.storage.levelDb.versionalLevelDB

import cats.instances.all._
import cats.syntax.semigroup._
import com.google.common.primitives.Longs
import com.typesafe.scalalogging.StrictLogging
import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion._
import encry.utils.{BalanceCalculator, ByteStr}
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.{AssetBox, DataBox, EncryBaseBox, TokenIssuingBox}
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.utils.{Algos, TaggedTypes}
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, ModifierId}
import org.iq80.leveldb.DB
import supertagged.@@

import scala.util.Success

case class WalletVersionalLevelDB(db: DB, settings: LevelDBSettings) extends StrictLogging with AutoCloseable {

  import WalletVersionalLevelDBCompanion._

  val levelDb: VersionalLevelDB = VersionalLevelDB(db, settings)

  //todo: optimize this
  def getAllBoxes(maxQty: Int = -1): Seq[EncryBaseBox] = levelDb.getAll(maxQty)
    .filterNot(_._1 sameElements BALANCE_KEY)
    .map { case (key, bytes) => StateModifierSerializer.parseBytes(bytes, key.head) }
    .collect { case Success(box) => box }

   def getBoxesByPredicate[BXT](f: List[BXT] => Boolean): Unit = {
     val keys: List[VersionalLevelDbKey] = levelDb.getCurrentElementsKeys().filterNot(_.sameElements(BALANCE_KEY))
     @scala.annotation.tailrec
     def loop(acc: List[BXT], newKeys: List[VersionalLevelDbKey]): List[BXT] = {
       (for {
         nextKey <- newKeys.headOption
         nextBox <- getBoxById(ADKey @@ nextKey.untag(VersionalLevelDbKey))
       } yield nextBox) match {
         case Some(box: BXT) =>
           val updatedAcc: List[BXT] = box :: acc
           if (f(updatedAcc)) acc else loop(updatedAcc, newKeys.drop(1))
         case Some(_) => loop(acc, newKeys.drop(1))
         case None if newKeys.nonEmpty => loop(acc, newKeys.drop(1))
         case None => acc
       }
     }
   }

  def getBoxById(id: ADKey): Option[EncryBaseBox] = levelDb.get(VersionalLevelDbKey @@ id.untag(ADKey))
    .flatMap(StateModifierSerializer.parseBytes(_, id.head).toOption)

  def getTokenBalanceById(id: TokenId): Option[Amount] = getBalances
    .find(_._1._2 == Algos.encode(id))
    .map(_._2)

  def containsBox(id: ADKey): Boolean = getBoxById(id).isDefined

  def rollback(modId: ModifierId): Unit = levelDb.rollbackTo(LevelDBVersion @@ modId.untag(ModifierId))

  def updateWallet(modifierId: ModifierId, newBxs: Seq[EncryBaseBox], spentBxs: Seq[EncryBaseBox],
                   intrinsicTokenId: ADKey): Unit = {
    val bxsToInsert: Seq[EncryBaseBox] = newBxs.filter(bx => !spentBxs.contains(bx))
    val boxesToInsert: (List[DataBox], List[TokenIssuingBox], List[AssetBox]) =
      newBxs.foldLeft(List.empty[DataBox], List.empty[TokenIssuingBox], List.empty[AssetBox]) {
        case ((dataBoxes, tokenIssuingBoxes, assetBoxes), nextBox: AssetBox) if !spentBxs.contains(nextBox) =>
          (dataBoxes, tokenIssuingBoxes, nextBox :: assetBoxes)
        case ((dataBoxes, tokenIssuingBoxes, assetBoxes), nextBox: TokenIssuingBox) if !spentBxs.contains(nextBox) =>
          (dataBoxes, nextBox :: tokenIssuingBoxes, assetBoxes)
        case ((dataBoxes, tokenIssuingBoxes, assetBoxes), nextBox: DataBox) if !spentBxs.contains(nextBox) =>
          (nextBox :: dataBoxes, tokenIssuingBoxes, assetBoxes)
        case ((dataBoxes, tokenIssuingBoxes, assetBoxes), nextBox) => (dataBoxes, tokenIssuingBoxes, assetBoxes)
    }
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
    levelDb.insert(
      LevelDbDiff(
        LevelDBVersion @@ modifierId.untag(ModifierId),
        newBalanceKeyValue :: bxsToInsert.map((bx: EncryBaseBox) =>
          (VersionalLevelDbKey @@ bx.id.untag(ADKey), VersionalLevelDbValue @@ bx.bytes)
        ).toList,
        spentBxs.map(elem => VersionalLevelDbKey @@ elem.id.untag(ADKey))
      )
    )
  }

  def getBalances: Map[(String, String), Amount] =
    levelDb.get(BALANCE_KEY)
      .map(_.sliding(72, 72)
        .map(ch => (Algos.encode(ch.take(32)), Algos.encode(ch.slice(32, 64))) -> Longs.fromByteArray(ch.takeRight(8)))
        .toMap).getOrElse(Map.empty)

  private def formIdsList(input: Option[VersionalLevelDbValue]): List[ADKey] =
    input
      .map(_.grouped(32).map(ADKey @@ _).toList)
      .getOrElse(List.empty[ADKey])

  private def getAssetBoxesIds: List[ADKey] = formIdsList(levelDb.get(ASSET_BOX_KEY))

  private def getDataBoxesIds: List[ADKey] = formIdsList(levelDb.get(DATA_BOX_KEY))

  private def getTokenIssueBoxesIds: List[ADKey] = formIdsList(levelDb.get(TOKEN_ISSUE_BOX_KEY))

  override def close(): Unit = levelDb.close()
}

object WalletVersionalLevelDBCompanion extends StrictLogging {

  val ASSET_BOX_KEY: VersionalLevelDbKey = VersionalLevelDbKey @@ Algos.hash("ASSET_BOX_KEY")
  val DATA_BOX_KEY: VersionalLevelDbKey = VersionalLevelDbKey @@ Algos.hash("DATA_BOX_KEY")
  val TOKEN_ISSUE_BOX_KEY: VersionalLevelDbKey = VersionalLevelDbKey @@ Algos.hash("TOKEN_ISSUE_BOX_KEY")
  val BALANCE_KEY: VersionalLevelDbKey = VersionalLevelDbKey @@ Algos.hash("BALANCE_KEY")

  val INIT_MAP: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map(
    BALANCE_KEY -> VersionalLevelDbValue @@ Array.emptyByteArray
  )

  def apply(levelDb: DB, settings: LevelDBSettings): WalletVersionalLevelDB = {
    val db = WalletVersionalLevelDB(levelDb, settings)
    db.levelDb.recoverOrInit(INIT_MAP)
    db
  }
}
