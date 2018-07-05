package encry.network

import akka.persistence.{PersistentActor, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
import encry.EncryApp._
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.{EncryPersistentModifier, NodeViewModifier}
import encry.network.ModifiersHolder.{UpdateBestHeaderHeight, _}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.Algos
import encry.utils.Logging
import encry.view.EncryNodeViewHolder.ReceivableMessages.{ApplyModifier, LocallyGeneratedModifier}
import encry.{ModifierId, ModifierTypeId}

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

class ModifiersHolder extends PersistentActor with Logging {

  var payloads: Set[EncryBlockPayload] = Set.empty
  var blocks: Seq[EncryBlock] = Seq.empty
  var modsFromRemote: Mods = Mods(Map.empty, 0)
  var amount: Amount = Amount(0, 0, 0)
  var bestHeaderHeight: Int = Int.MaxValue
  var bestBlockHeight: Int = Int.MaxValue

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
    amount = Amount(headers.size, payloadCache.size, completedBlocks.size)
    saveSnapshot(amount)
    logger.info(s"ModifiersHolder: ${amount.headers} : ${
      if (headers.nonEmpty) {
        Algos.encode(headers.last._2.id) + "---" + headerCache.last._2.height
      }
    } - ${amount.payloads} - ${amount.blocks}")
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
    case CheckHeaders => headers.foldLeft(SortedMap[Int, EncryBlockHeader]()) {

      case (headersToApply, (height, header)) =>
        if (height < bestHeaderHeight || height <= headersToApply.last._1 + 1) headersToApply + (header.height -> header)
        else headersToApply
    }.foreach { headerInfo =>
        nodeViewHolder ! ApplyModifier(headerInfo._2)
        headers -= headerInfo._1
      }

    case CheckCompletedBlocks => completedBlocks.foldLeft(SortedMap[Int, EncryBlock]()) {

      case (blockToApply, (height, block)) =>
        if (height < bestBlockHeight || height <= blockToApply.last._1 + 1) blockToApply + (block.header.height -> block)
        else blockToApply
    }.foreach { headerInfo =>
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

  def createBlockIfPossible(payloadId: ModifierId): Unit = {
    logger.info(s"Trying too create block with payload: ${Algos.encode(payloadId)}")
    notCompletedBlocks.get(payloadId).foreach(headerId => headerCache.get(headerId).foreach { header =>
      payloadCache.get(payloadId).foreach { payload =>
        logger.info(s"Create succes on height: ${header.height} with ${Algos.encode(headerId)}")
        completedBlocks += header.height -> EncryBlock(header, payload, None)
        notCompletedBlocks -= payloadId
        payloadCache -= payloadId
        headerCache -= headerId
      }
    })
  }

  def updateModifiers(modsTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier]): Unit = modifiers.foreach {

    case header: EncryBlockHeader =>
      if (!notCompletedBlocks.contains(header.payloadId)) {
        notCompletedBlocks += header.payloadId -> header.id
        headers += header.height -> header
        headerCache += header.id -> header
      } else {
        notCompletedBlocks = notCompletedBlocks - header.payloadId + (header.payloadId -> header.id)
        createBlockIfPossible(header.payloadId)
      }
    case payload: EncryBlockPayload =>
      logger.info(s"Get payload of id: ${Algos.encode(payload.id)}")
      logger.info(s"not completedBlocks contains it: ${!notCompletedBlocks.contains(payload.id)}")
      if (!notCompletedBlocks.contains(payload.id)) {
        notCompletedBlocks += payload.id -> ModifierId @@ Array.emptyByteArray
        payloadCache += payload.id -> payload
      } else createBlockIfPossible(payload.id)
    case block: EncryBlock => completedBlocks += block.header.height -> block
    case _ =>
  }
}

object ModifiersHolder {

  case object CheckCompletedBlocks

  case object CheckHeaders

  case class UpdateBestHeaderHeight(height: Int)

  case class UpdateBestBlockHeight(height: Int)

  case class Amount(headers: Int, payloads: Int, blocks: Int)

  case class RequestedModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier])

  case class Mods(numberOfModsByPeerAndModType: Map[(ConnectedPeer, ModifierTypeId), Int], numberOfPacksFromRemotes: Int)

}