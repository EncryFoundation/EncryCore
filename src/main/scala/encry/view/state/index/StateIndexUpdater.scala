package encry.view.state.index

import akka.actor.{Actor, ActorRef}
import com.google.common.primitives.Ints
import encry.account.Address
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.state.box.{AssetBox, OpenBox}
import encry.settings.{Algos, Constants, EncryAppSettings}
import encry.view.history.Height
import encry.view.state.EncryState
import encry.view.state.index.StateIndexReader.{keyByAddress, openBoxesAddress}
import encry.view.state.index.storage.StateIndexStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.NodeViewHolder.{SemanticallySuccessfulModifier, Subscribe}
import scorex.core.utils.ScorexLogging
import scorex.core.{ModifierId, NodeViewHolder}
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58

import scala.collection.mutable

class StateIndexUpdater(viewHolderRef: ActorRef, settings: EncryAppSettings)
  extends Actor with ScorexLogging {

  import StateIndexUpdater._

  val indexStore: Store = new LSMStore(EncryState.getIndexDir(settings),
    keepVersions = Constants.Store.indexKeepVersions)

  protected lazy val indexStorage = new StateIndexStorage(indexStore)

  override def preStart(): Unit = {
    viewHolderRef ! Subscribe(Seq(NodeViewHolder.EventType.SuccessfulSemanticallyValidModifier))
  }

  override def receive: Receive = {

    case SemanticallySuccessfulModifier(mod) =>
      mod match {
        case block: EncryBlock =>
          val stateOpsMap = mutable.HashMap.empty[Address, (mutable.Set[ADKey], mutable.Set[ADKey])]
          block.payload.transactions.foreach { tx =>
            tx.useBoxes.foreach { id =>
              id.head match {
                case AssetBox.typeId =>
                  stateOpsMap.get(Address @@ tx.proposition.address) match {
                    case Some(t) =>
                      // FIXME: Usage of `exists()`.
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

        case payload: EncryBlockPayload =>

        case _ => log.debug("Got unhandled modifier")
      }
  }

  // Updates or creates index for key `address`.
  def updateIndexFor(address: Address, toRemove: Seq[ADKey], toInsert: Seq[ADKey]): Unit = {
    val addrKey = keyByAddress(address)
    val bxsOpt = indexStorage.boxIdsByAddress(address)
    bxsOpt match {
      case Some(bxs) =>
        indexStorage.update(
          deriveVersionId(addrKey ++ toRemove.head ++ toInsert.head),
          Seq(ByteArrayWrapper(addrKey)),
          Seq(ByteArrayWrapper(addrKey) -> ByteArrayWrapper(
            (bxs.filterNot(toRemove.contains) ++ toInsert)
              .foldLeft(Array[Byte]()) { case (buff, id) => buff ++ id }
          ))
        )
      case None =>
        indexStorage.update(
          deriveVersionId(addrKey ++ toRemove.head ++ toInsert.head),
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

    log.info(s"Updating StateIndex for mod: ${Base58.encode(version)}")

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
    indexStorage.update(deriveVersionId(version), keysToRemove, Seq())

    // Than insert new versions of records + new records.
    indexStorage.update(version, Seq(), recordsToInsert)
  }
}

object StateIndexUpdater {

  def deriveVersionId(bytes: Array[Byte]): ModifierId = ModifierId @@ Algos.hash(bytes)
}
