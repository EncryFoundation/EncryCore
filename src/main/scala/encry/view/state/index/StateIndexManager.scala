package encry.view.state.index

import com.google.common.primitives.Ints
import encry.account.{Account, Address}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.{AssetBox, OpenBox, PubKeyInfoBox}
import encry.settings.{Algos, Constants}
import encry.view.history.Height
import encry.view.state.index.storage.StateIndexStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.utils.ScorexLogging
import scorex.core.{ModifierId, VersionTag}
import scorex.crypto.authds.ADKey

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

trait StateIndexManager extends ScorexLogging {

  import StateIndexManager._

  val indexStore: Store

  protected lazy val indexStorage = new StateIndexStorage(indexStore)

  def boxIdsByAddress(addr: Address): Option[Seq[ADKey]] = indexStorage.boxIdsByAddress(addr)

  def updateIndex(mod: EncryPersistentModifier): Future[Unit] = Future {
    mod match {
      case block: EncryBlock =>
        bulkUpdateIndex(
          block.header.id,
          toIndexModificationsMap(block.payload.transactions),
          Some(Height @@ block.header.height)
        )
      case payload: EncryBlockPayload =>
        bulkUpdateIndex(
          payload.headerId,
          toIndexModificationsMap(payload.transactions),
          None
        )
      case _ => // Do nothing.
    }
  }

  // TODO: Refactoring. Too many lines of code in one method.
  private def toIndexModificationsMap(txs: Seq[EncryBaseTransaction]) = {
    val modifications = mutable.TreeMap.empty[Address, (mutable.Set[ADKey], mutable.Set[ADKey])]
    txs.foreach { tx =>
      tx.useBoxes.foreach { id =>
        id.head match {
          case AssetBox.TypeId =>
            modifications.get(Address @@ tx.accountPubKey.address) match {
              case Some(t) =>
                if (t._2.exists(_.sameElements(id))) t._2.remove(id)
                else t._1.add(id)
              case None =>
                modifications.update(
                  Address @@ tx.accountPubKey.address, mutable.Set(id) -> mutable.Set.empty[ADKey])
            }
          case OpenBox.typeId =>
            modifications.get(openBoxesAddress) match {
              case Some(t) =>
                if (t._2.exists(_.sameElements(id))) t._2.remove(id)
                else t._1.add(id)
              case None =>
                modifications.update(openBoxesAddress, mutable.Set(id) -> mutable.Set.empty[ADKey])
            }
        }
      }
      tx.newBoxes.foreach {
        case bx: AssetBox =>
          modifications.get(bx.proposition.account.address) match {
            case Some(t) => t._2.add(bx.id)
            case None => modifications.update(
              bx.proposition.account.address, mutable.Set.empty[ADKey] -> mutable.Set(bx.id))
          }
        case bx: PubKeyInfoBox =>
          modifications.get(bx.proposition.account.address) match {
            case Some(t) => t._2.add(bx.id)
            case None => modifications.update(
              bx.proposition.account.address, mutable.Set.empty[ADKey] -> mutable.Set(bx.id))
          }
        case bx: OpenBox =>
          modifications.get(openBoxesAddress) match {
            case Some(t) => t._2.add(bx.id)
            case None => modifications.update(openBoxesAddress, mutable.Set.empty[ADKey] -> mutable.Set(bx.id))
          }
      }
    }
    modifications.toIndexedSeq
  }

  def bulkUpdateIndex(version: ModifierId,
                      modifications: IndexedSeq[(Address, (mutable.Set[ADKey], mutable.Set[ADKey]))],
                      modHeightOpt: Option[Height]): Unit = {
    val modSummary = modifications
      .foldLeft(Seq[(Address, Seq[ADKey])](), Seq[(Address, Seq[ADKey])]()) {
        case ((bNew, bExs), (addr, (toRem, toIns))) =>
          val bxsOpt = indexStorage.boxIdsByAddress(addr)
          bxsOpt match {
            case Some(bxs) =>
              val bxsToReInsert = bxs.foldLeft(Seq[ADKey]()) {
                case (buff, id) => if (toRem.forall(!_.sameElements(id))) buff :+ id else buff }
              bNew -> (bExs :+ (addr, bxsToReInsert ++ toIns.toSeq))
            case None =>
              (bNew :+ (addr, toIns.toSeq)) -> bExs
          }
      }

    log.info(s"Updating StateIndex for mod: ${Algos.encode(version)}")

    val toRemove = modHeightOpt.map(_ => modSummary._2.map(i => keyByAddress(i._1)) :+ stateHeightKey)
      .getOrElse(modSummary._2.map(i => keyByAddress(i._1)))

    val toInsert = {
      val bxsOps = (modSummary._1 ++ modSummary._2).map { case (addr, ids) =>
        keyByAddress(addr) -> ByteArrayWrapper(ids.foldLeft(Array[Byte]())(_ ++ _))
      }
      modHeightOpt.map(_ => bxsOps :+ (stateHeightKey, ByteArrayWrapper(Ints.toByteArray(modHeightOpt.get))))
        .getOrElse(bxsOps)
    }

    indexStorage.updateWithReplacement(version, toRemove, toInsert)
  }

  protected def rollbackIndex(to: VersionTag): Try[Unit] = {
    val wrappedVersion = ByteArrayWrapper(to)
    Try(indexStore.rollback(wrappedVersion))
      .map(_ => indexStore.clean(Constants.keepVersions))
  }
}

object StateIndexManager {

  // OpenBoxes are stored in index under this addr.
  val openBoxesAddress: Address = Address @@ "4kYwt36bmG7U9Stumey79nYN7NEEV4VEibsXWnce7RUT4FRaM3"

  val stateHeightKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash("state_height".getBytes))

  def keyByAddress(address: Address): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash(Account.decodeAddress(address)))
}
