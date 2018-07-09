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

  var amount: Amount = Amount(0, 0, 0, 0, 0, Seq.empty, Seq.empty)

  /**
    * Map, which contains not completed blocks
    * Key can be payloadId or also header id. So value depends on key, and can contains: headerId, payloadId
    */
  var notCompletedBlocks: Map[String, String] = Map.empty
  var headerCache: Map[String, (EncryBlockHeader, Int)] = Map.empty
  var payloadCache: Map[String, (EncryBlockPayload, Int)] = Map.empty
  var completedBlocks: SortedMap[Int, EncryBlock] = SortedMap.empty

  context.system.scheduler.schedule(10.second, 10.second) {
    amount = Amount(headerCache.size,
      payloadCache.size,
      notCompletedBlocks.size,
      completedBlocks.size,
      if (completedBlocks.nonEmpty) completedBlocks.keys.max else 0,
      countGaps(completedBlocks.keys.toSeq),
      headerCache.values.filter(_._2 > 1).map( headerWithDuplicatesQty => headerWithDuplicatesQty._1.id -> headerWithDuplicatesQty._2).toSeq ++
        payloadCache.values.filter(_._2 > 1).map( payloadWithDuplicatesQty => payloadWithDuplicatesQty._1.id -> payloadWithDuplicatesQty._2).toSeq
    )
    saveSnapshot(amount)
    logger.info(s"${amount.receivedHeaders} headers received - " +
      s" ${amount.receivedPayloads} payloads received - " +
      s" ${amount.notCompletedBlocks} not full blocks - " +
      s" ${amount.completedBlocks} full blocks - " +
      s"max height: ${amount.maxHeight} " +
      s"Gaps: ${amount.gaps.foldLeft("") { case (str, gap) => str + s"(${gap._1}, ${gap._2})" }} " +
      s"Duplicates: ${amount.duplicates.foldLeft(""){ case (str, duplicate) => str + s"(${Algos.encode(duplicate._1)}, ${duplicate._2})"  }}")
  }

  override def preStart(): Unit = logger.info(s"ModifiersHolder actor is started.")

  override def receiveRecover: Receive = {
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
        completedBlocks += header._1.height -> EncryBlock(header._1, payload._1, None)
        notCompletedBlocks -= Algos.encode(payloadId)
      }
    })
  }

  def updateModifiers(modsTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier]): Unit = modifiers.foreach {

    case header: EncryBlockHeader =>
      val prevValue: (EncryBlockHeader, Int) = headerCache.getOrElse(Algos.encode(header.id), (header, -1))
      headerCache += Algos.encode(header.id) -> (prevValue._1, prevValue._2 + 1)
      if (!notCompletedBlocks.contains(Algos.encode(header.payloadId)))
        notCompletedBlocks += Algos.encode(header.payloadId) -> Algos.encode(header.id)
      else {
        notCompletedBlocks = (notCompletedBlocks - Algos.encode(header.payloadId)) + (Algos.encode(header.payloadId) -> Algos.encode(header.id))
        createBlockIfPossible(header.payloadId)
      }
    case payload: EncryBlockPayload =>
      val prevValue: (EncryBlockPayload, Int) = payloadCache.getOrElse(Algos.encode(payload.id), (payload, -1))
      payloadCache += Algos.encode(payload.id) -> (prevValue._1, prevValue._2 + 1)
      if (!notCompletedBlocks.contains(Algos.encode(payload.id))) notCompletedBlocks += Algos.encode(payload.id) -> ""
      else createBlockIfPossible(payload.id)
    case block: EncryBlock => completedBlocks += block.header.height -> block
    case _ =>
  }
}

object ModifiersHolder {

  case class Amount(receivedHeaders: Int,
                    receivedPayloads: Int,
                    notCompletedBlocks: Int,
                    completedBlocks: Int,
                    maxHeight: Int,
                    gaps: Seq[(Int, Int)],
                    duplicates: Seq[(ModifierId, Int)])

  case class RequestedModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier])

  case class Mods(numberOfModsByPeerAndModType: Map[(ConnectedPeer, ModifierTypeId), Int], numberOfPacksFromRemotes: Int)

  def countGaps(blocksHeight: Seq[Int]): Seq[(Int, Int)] = blocksHeight.foldLeft(Seq[(Int, Int)](), 0) {
    case ((gaps, prevHeight), blockHeight) =>
      if (prevHeight + 1 != blockHeight) (gaps :+ (prevHeight + 1, blockHeight - 1), blockHeight)
      else (gaps, blockHeight)
  }._1
}