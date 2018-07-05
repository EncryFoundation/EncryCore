package encry.network

import akka.persistence.{PersistentActor, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
import encry.EncryApp._
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.{EncryPersistentModifier, NodeViewModifier}
import encry.network.ModifiersHolder.{UpdateBestHeaderHeight, _}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.utils.Logging
import encry.view.EncryNodeViewHolder.ReceivableMessages.{ApplyModifier, LocallyGeneratedModifier}
import encry.view.history.Height
import encry.{ModifierId, ModifierTypeId}

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

class ModifiersHolder extends PersistentActor with Logging {

  var payloads: Set[EncryBlockPayload] = Set.empty
  var blocks: Seq[EncryBlock] = Seq.empty
  var modsFromRemote: Mods = Mods(Map.empty, 0)
  var amount: Amount = Amount(0, 0, 0)
  var bestHeaderHeight: Height = Height @@ Int.MaxValue
  var bestBlockHeight: Height = Height @@ Int.MaxValue

  /**
    * Map, which contains not completed blocks
    * Key can be payloadId or also header id. So value depends on key, and can contains: headerId, payloadId
    */
  var notCompletedBlocks: Map[ModifierId, ModifierId] = Map.empty
  var headerCache: Map[ModifierId, EncryBlockHeader] = Map.empty
  var headers: SortedMap[Int, EncryBlockHeader] = SortedMap.empty
  var payloadCache: Map[ModifierId, EncryBlockPayload] = Map.empty
  var completedBlocks: SortedMap[Int, EncryBlock] = SortedMap.empty

  context.system.scheduler.schedule(10.second, 10.second) {
    amount = Amount(headers.size, payloads.size, blocks.size)
    saveSnapshot(amount)
    logger.info(s"ModifiersHolder: ${amount.headers} - ${amount.payloads} - ${amount.blocks}")
    self ! CheckCompletedBlocks
    self ! CheckHeaders
  }

  override def preStart(): Unit = logger.info(s"ModifiersHolder actor is started.")

  override def receiveRecover: Receive = {
    case SnapshotOffer(_, snapshot: Mods) => modsFromRemote = snapshot
    case SnapshotOffer(_, snapshot: Amount) => amount = snapshot
  }

  override def receiveCommand: Receive = {
    case SaveSnapshotSuccess(metadata) => logger.info("Success with snapshot")
    case SaveSnapshotFailure(metadata, reason) => logger.info("Failure with snapshot")
    case UpdateBestHeaderHeight(height) => bestHeaderHeight = height
    case UpdateBestBlockHeight(height) => bestBlockHeight = height
    case CheckHeaders => headers.foldLeft(Seq[Int, EncryBlockHeader]()) {

      case (headersToApply, (height, header)) =>
        if (height < bestHeaderHeight || height <= headersToApply.max.height + 1) headersToApply :+ header
        else headersToApply
      }
      headers.foreach{headerInfo =>
        nodeViewHolder ! ApplyModifier(headerInfo._2)
        headers -= headerInfo._1
      }

    case CheckCompletedBlocks => completedBlocks.foldLeft(Seq[Int, EncryBlock]()) {

      case (blockToApply, (height, block)) =>
        if (height < bestBlockHeight || height <= blockToApply.max.header.height + 1) blockToApply :+ block
        else blockToApply
    }
      headers.foreach{headerInfo =>
        nodeViewHolder ! ApplyModifier(headerInfo._2)
        completedBlocks -= headerInfo._1
      }

    case RequestedModifiers(modifierTypeId, modifiers) => updateModifiers(modifierTypeId, modifiers)
    case lm: LocallyGeneratedModifier[EncryPersistentModifier] => updateModifiers(lm.pmod.modifierTypeId, Seq(lm.pmod))
    case x: Any => logger.info(s"Strange input: $x")
  }

  override def persistenceId: String = "persistent actor"

  override def journalPluginId: String = "akka.persistence.journal.leveldb"

  override def snapshotPluginId: String = "akka.persistence.snapshot-store.local"

  def createBlockIfPossible(payloadId: ModifierId): Unit =
    notCompletedBlocks.get(payloadId).foreach(headerId => headerCache.get(headerId).foreach { header =>
      payloadCache.get(payloadId).foreach{ payload =>
        completedBlocks += header.height -> EncryBlock(header, payload, None)
        notCompletedBlocks -= payloadId
        payloadCache -= payloadId
        headerCache -= headerId
      }
    })


  def updateModifiers(modsTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier]): Unit = modifiers.foreach {

    case header: EncryBlockHeader =>
      if (!notCompletedBlocks.contains(header.payloadId)) {
        notCompletedBlocks += header.payloadId -> header.id
        headers += header.height -> header
        headerCache += header.id -> header
      } else createBlockIfPossible(header.payloadId)
    case payload: EncryBlockPayload =>
      if (!notCompletedBlocks.contains(payload.id)) {
        notCompletedBlocks += payload.id -> Seq.empty
        payloadCache += payload.id -> payload
      } else createBlockIfPossible(payload.id)
    case block: EncryBlock => completedBlocks += block.header.height -> block
    case _ =>
  }
}

object ModifiersHolder {

  case object CheckCompletedBlocks

  case object CheckHeaders

  case class UpdateBestHeaderHeight(height: Height)

  case class UpdateBestBlockHeight(height: Height)

  case class Amount(headers: Int, payloads: Int, blocks: Int)

  case class RequestedModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier])

  case class Mods(numberOfModsByPeerAndModType: Map[(ConnectedPeer, ModifierTypeId), Int], numberOfPacksFromRemotes: Int)

}