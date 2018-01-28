package encry.view.state.index

import com.google.common.primitives.Ints
import encry.account.Address
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.state.box.{AssetBox, OpenBox}
import encry.modifiers.state.box.proposition.AddressProposition
import encry.settings.Algos
import encry.view.history.Height
import encry.view.state.index.storage.StateIndexStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable
import scala.concurrent.Future

trait StateIndexReader extends ScorexLogging {

  import StateIndexReader._

  val indexStore: Store

  protected lazy val indexStorage = new StateIndexStorage(indexStore)

  def boxIdsByAddress(addr: Address): Option[Seq[ADKey]] = indexStorage.boxesByAddress(addr)

  def updateIndexOn(mod: EncryPersistentModifier): Future[Unit] = Future {

    mod match {
      case block: EncryBlock =>
        val stateOpsMap = mutable.HashMap.empty[Address, (mutable.Set[ADKey], mutable.Set[ADKey])]
        block.payload.transactions.foreach { tx =>
          tx.useBoxes.foreach { id =>
            id.head match {
              case AssetBox.typeId =>
                stateOpsMap.get(Address @@ tx.proposition.address) match {
                  case Some(t) =>
                    if (t._2.exists(_.sameElements(id))) t._2.remove(id)
                    else t._1.add(id)
                  case None =>
                    stateOpsMap.update(
                      Address @@ tx.proposition.address, mutable.Set(id) -> mutable.Set.empty[ADKey])
                }
              case OpenBox.typeId =>
                stateOpsMap.get(openBoxesAddress) match {
                  case Some(t) =>
                    if (t._2.exists(_.sameElements(id))) t._2.remove(id)
                    else t._1.add(id)
                  case None =>
                    stateOpsMap.update(openBoxesAddress, mutable.Set(id) -> mutable.Set.empty[ADKey])
                }
            }
          }
          tx.newBoxes.foreach {
            case bx: AssetBox =>
              stateOpsMap.get(bx.proposition.address) match {
                case Some(t) => t._2.add(bx.id)
                case None => stateOpsMap.update(
                  bx.proposition.address, mutable.Set.empty[ADKey] -> mutable.Set(bx.id))
              }
            case bx: OpenBox =>
              stateOpsMap.get(StateIndexReader.openBoxesAddress) match {
                case Some(t) => t._2.add(bx.id)
                case None => stateOpsMap.update(
                  StateIndexReader.openBoxesAddress, mutable.Set.empty[ADKey] -> mutable.Set(bx.id))
              }
          }
        }
        bulkUpdateIndex(block.header.id, stateOpsMap, Some(Height @@ mod.asInstanceOf[EncryBlock].header.height))

      case _ => // Do nothing.
    }
  }

  // Updates or creates index for key `address`.
  def updateIndexFor(address: Address, toRemove: Seq[ADKey], toInsert: Seq[ADKey]): Future[Unit] = Future {
    val addrKey = keyByAddress(address)
    val bxsOpt = indexStorage.boxesByAddress(address)
    bxsOpt match {
      case Some(bxs) =>
        indexStorage.update(
          ModifierId @@ Algos.hash(addrKey ++ toRemove.head ++ toInsert.head),
          Seq(ByteArrayWrapper(addrKey)),
          Seq(ByteArrayWrapper(addrKey) -> ByteArrayWrapper(
            (bxs.filterNot(toRemove.contains) ++ toInsert)
              .foldLeft(Array[Byte]()) { case (buff, id) => buff ++ id }
          ))
        )
      case None =>
        indexStorage.update(
          ModifierId @@ Algos.hash(addrKey ++ toRemove.head ++ toInsert.head),
          Seq(),
          Seq(ByteArrayWrapper(addrKey) ->
            ByteArrayWrapper(toInsert.foldLeft(Array[Byte]()) { case (buff, id) => buff ++ id }))
        )
    }
  }

  def bulkUpdateIndex(version: ModifierId,
                      opsMap: mutable.HashMap[Address, (mutable.Set[ADKey], mutable.Set[ADKey])],
                      modHeightOpt: Option[Height]): Unit = {
    val opsFinal = opsMap
      .foldLeft(Seq[(Address, Seq[ADKey])](), Seq[(Address, Seq[ADKey])]()) {
        case ((bNew, bExs), (addr, (toRem, toIns))) =>
          val bxsOpt = indexStorage.boxesByAddress(addr)
          bxsOpt match {
            case Some(bxs) =>
              val bxsToReInsert = bxs.foldLeft(Seq[ADKey]()) {
                case (buff, id) => if (toRem.forall(!_.sameElements(id))) buff :+ id else buff }
              bNew -> (bExs :+ (addr, bxsToReInsert ++ toIns.toSeq))
            case None =>
              (bNew :+ (addr, toIns.toSeq)) -> bExs
          }
      }

    log.info(s"Updating index for mod: ${Base58.encode(version)}")

    val keysToRemove = {
      if (modHeightOpt.isDefined)
        opsFinal._2.map(i => ByteArrayWrapper(keyByAddress(i._1))) :+ StateIndexReader.stateHeightKey
      else
        opsFinal._2.map(i => ByteArrayWrapper(keyByAddress(i._1)))
    }

    val recordsToInsert = {
      val bxsOps = (opsFinal._1 ++ opsFinal._2).map { case (addr, ids) =>
        ByteArrayWrapper(keyByAddress(addr)) ->
          ByteArrayWrapper(ids.foldLeft(Array[Byte]()) { case (buff, id) => buff ++ id })
      }
      if (modHeightOpt.isDefined)
        bxsOps :+ (StateIndexReader.stateHeightKey, ByteArrayWrapper(Ints.toByteArray(modHeightOpt.get)))
      else
        bxsOps
    }

    // First remove existing records assoc with addresses to be updated.
    indexStorage.update(ModifierId @@ Algos.hash(version), keysToRemove, Seq())

    // Than insert new versions of records + new records.
    indexStorage.update(version, Seq(), recordsToInsert)
  }
}

object StateIndexReader {

  val openBoxesAddress: Address = Address @@ "3goCpFrrBakKJwxk7d4oY5HN54dYMQZbmVWKvQBPZPDvbL3hHp"

  val stateHeightKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash("state_height".getBytes))

  def keyByAddress(address: Address) = Algos.hash(AddressProposition.getAddrBytes(address))
}
