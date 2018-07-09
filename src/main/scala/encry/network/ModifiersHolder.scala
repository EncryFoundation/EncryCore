package encry.network

import akka.persistence.{PersistentActor, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
import encry.EncryApp._
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.{EncryPersistentModifier, NodeViewModifier}
import encry.network.ModifiersHolder._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.Algos
import encry.utils.Logging
import encry.view.EncryNodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import encry.{ModifierId, ModifierTypeId}

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

class ModifiersHolder extends PersistentActor with Logging {

  var modsFromRemote: Mods = Mods(Map.empty, 0)
  var amount: Amount = Amount(0, 0, 0, 0, 0, Seq.empty)

  /**
    * Map, which contains not completed blocks
    * Key can be payloadId or also header id. So value depends on key, and can contains: headerId, payloadId
    */
  var notCompletedBlocks: Map[String, String] = Map.empty
  var headerCache: Map[String, EncryBlockHeader] = Map.empty
  var payloadCache: Map[String, EncryBlockPayload] = Map.empty
  var completedBlocks: SortedMap[Int, EncryBlock] = SortedMap.empty

  context.system.scheduler.schedule(10.second, 10.second) {
    amount = Amount(headerCache.size,
      payloadCache.size,
      notCompletedBlocks.size,
      completedBlocks.size,
      completedBlocks.keys.max,
      gaps
    )
    saveSnapshot(amount)
    logger.info(s"${amount.receivedHeaders} headers received - " +
      s"${amount.receivedPayloads} payloads received - " +
      s"${amount.notCompletedBlocks} not full blocks - " +
      s"${amount.completedBlocks} full blocks - " +
      s"max height: ${amount.maxHeight} " +
      s"Gaps: ${amount.gaps.foldLeft(""){ case (str, gap) => str + s"(${gap._1}, ${gap._2})"}}")
  }

  override def preStart(): Unit = logger.info(s"ModifiersHolder actor is started.")

  override def receiveRecover: Receive = {
    case SnapshotOffer(_, snapshot: Mods) => modsFromRemote = snapshot
    case SnapshotOffer(_, snapshot: Amount) => amount = snapshot
  }

  override def receiveCommand: Receive = {
    case SaveSnapshotSuccess(metadata) => logger.info("Success with snapshot")
    case SaveSnapshotFailure(metadata, reason) => logger.info("Failure with snapshot")
    case RequestedModifiers(modifierTypeId, modifiers) => updateModifiers(modifierTypeId, modifiers)
    case lm: LocallyGeneratedModifier[EncryPersistentModifier] => updateModifiers(lm.pmod.modifierTypeId, Seq(lm.pmod))
    case x: Any => logger.info(s"Strange input: $x")
  }

  override def persistenceId: String = "persistent actor"

  override def journalPluginId: String = "akka.persistence.journal.leveldb"

  override def snapshotPluginId: String = "akka.persistence.snapshot-store.local"

  def createBlockIfPossible(payloadId: ModifierId): Unit = {
    notCompletedBlocks.get(Algos.encode(payloadId)).foreach(headerId => headerCache.get(headerId).foreach { header =>
      payloadCache.get(Algos.encode(payloadId)).foreach { payload =>
        completedBlocks += header.height -> EncryBlock(header, payload, None)
        notCompletedBlocks -= Algos.encode(payloadId)
      }
    })
  }

  def updateModifiers(modsTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier]): Unit = modifiers.foreach {

    case header: EncryBlockHeader =>
      headerCache += Algos.encode(header.id) -> header
      if (!notCompletedBlocks.contains(Algos.encode(header.payloadId)))
        notCompletedBlocks += Algos.encode(header.payloadId) -> Algos.encode(header.id)
      else {
        notCompletedBlocks = (notCompletedBlocks - Algos.encode(header.payloadId)) + (Algos.encode(header.payloadId) -> Algos.encode(header.id))
        createBlockIfPossible(header.payloadId)
      }
    case payload: EncryBlockPayload =>
      payloadCache += Algos.encode(payload.id) -> payload
      if (!notCompletedBlocks.contains(Algos.encode(payload.id))) notCompletedBlocks += Algos.encode(payload.id) -> ""
      else createBlockIfPossible(payload.id)
    case block: EncryBlock => completedBlocks += block.header.height -> block
    case _ =>
  }

  def gaps: Seq[(Int, Int)] = completedBlocks.keys.foldLeft(Seq[(Int, Int)](), 0) {
      case ((gaps, prevHeight), blockHeight) =>
        if (prevHeight + 1 != blockHeight) (gaps :+ (prevHeight, blockHeight - 1), blockHeight)
        else (gaps, blockHeight)
    }._1
}

object ModifiersHolder {

  case class Amount(receivedHeaders: Int,
                    receivedPayloads: Int,
                    notCompletedBlocks: Int,
                    completedBlocks: Int,
                    maxHeight: Int,
                    gaps: Seq[(Int, Int)])

  case class RequestedModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier])

  case class Mods(numberOfModsByPeerAndModType: Map[(ConnectedPeer, ModifierTypeId), Int], numberOfPacksFromRemotes: Int)
}