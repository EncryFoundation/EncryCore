package encry.view.state.index

import com.google.common.primitives.Ints
import encry.account.Address
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.{AssetBox, OpenBox, PubKeyInfoBox}
import encry.modifiers.state.box.proposition.AddressProposition
import encry.settings.Algos
import encry.view.history.Height
import encry.view.state.index.storage.StateIndexStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
    val idxModificationsMap = mutable.HashMap.empty[Address, (mutable.Set[ADKey], mutable.Set[ADKey])]
    txs.foreach { tx =>
      tx.useBoxes.foreach { id =>
        id.head match {
          case AssetBox.typeId =>
            idxModificationsMap.get(Address @@ tx.proposition.address) match {
              case Some(t) =>
                if (t._2.exists(_.sameElements(id))) t._2.remove(id)
                else t._1.add(id)
              case None =>
                idxModificationsMap.update(
                  Address @@ tx.proposition.address, mutable.Set(id) -> mutable.Set.empty[ADKey])
            }
          case OpenBox.typeId =>
            idxModificationsMap.get(openBoxesAddress) match {
              case Some(t) =>
                if (t._2.exists(_.sameElements(id))) t._2.remove(id)
                else t._1.add(id)
              case None =>
                idxModificationsMap.update(openBoxesAddress, mutable.Set(id) -> mutable.Set.empty[ADKey])
            }
        }
      }
      tx.newBoxes.foreach {
        case bx: AssetBox =>
          idxModificationsMap.get(bx.proposition.address) match {
            case Some(t) => t._2.add(bx.id)
            case None => idxModificationsMap.update(
              bx.proposition.address, mutable.Set.empty[ADKey] -> mutable.Set(bx.id))
          }
        case bx: PubKeyInfoBox =>
          idxModificationsMap.get(bx.proposition.address) match {
            case Some(t) => t._2.add(bx.id)
            case None => idxModificationsMap.update(
              bx.proposition.address, mutable.Set.empty[ADKey] -> mutable.Set(bx.id))
          }
        case bx: OpenBox =>
          idxModificationsMap.get(openBoxesAddress) match {
            case Some(t) => t._2.add(bx.id)
            case None => idxModificationsMap.update(openBoxesAddress, mutable.Set.empty[ADKey] -> mutable.Set(bx.id))
          }
      }
    }
    idxModificationsMap
  }

  def bulkUpdateIndex(version: ModifierId,
                      opsMap: mutable.HashMap[Address, (mutable.Set[ADKey], mutable.Set[ADKey])],
                      modHeightOpt: Option[Height]): Unit = {
    val opsFinal = opsMap
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

    val toRemove = {
      if (modHeightOpt.isDefined) opsFinal._2.map(i => keyByAddress(i._1)) :+ stateHeightKey
      else opsFinal._2.map(i => keyByAddress(i._1))
    }

    val toInsert = {
      val bxsOps = (opsFinal._1 ++ opsFinal._2).map { case (addr, ids) =>
        keyByAddress(addr) -> ByteArrayWrapper(ids.foldLeft(Array[Byte]())(_ ++ _))
      }
      if (modHeightOpt.isDefined) bxsOps :+ (stateHeightKey, ByteArrayWrapper(Ints.toByteArray(modHeightOpt.get)))
      else bxsOps
    }

    indexStorage.updateWithReplacement(version, toRemove, toInsert)
  }
}

object StateIndexManager {

  // OpenBoxes are stored in index under this addr for consensus.
  val openBoxesAddress: Address = Address @@ "3goCpFrrBakKJwxk7d4oY5HN54dYMQZbmVWKvQBPZPDvbL3hHp"

  val stateHeightKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash("state_height".getBytes))

  def keyByAddress(address: Address): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash(AddressProposition.getAddrBytes(address)))
}
