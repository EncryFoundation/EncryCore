package encry.network

import akka.persistence._
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.EncryApp._
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.{EncryPersistentModifier, NodeViewModifier}
import encry.network.ModifiersHolder._
import encry.stats.StatsSender.TransactionsStatMessage
import encry.view.EncryNodeViewHolder.ReceivableMessages.{BlocksFromLocalPersistence, LocallyGeneratedModifier}
import org.encryfoundation.common.Algos
import encry.utils.Logging
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._
import scala.language.postfixOps

class ModifiersHolder extends PersistentActor with Logging {

  var stat: Statistics = Statistics(0, 0, 0, 0, 0, Seq.empty, Seq.empty)
  var headers: Map[String, (EncryBlockHeader, Int)] = Map.empty
  var payloads: Map[String, (EncryBlockPayload, Int)] = Map.empty
  var nonCompletedBlocks: Map[String, String] = Map.empty
  var completedBlocks: SortedMap[Int, EncryBlock] = SortedMap.empty

  context.system.scheduler.schedule(10.second, 30.second) {
    logDebug(Statistics(headers, payloads, nonCompletedBlocks, completedBlocks).toString)
  }

  context.system.scheduler.scheduleOnce(5 seconds) {
    if (completedBlocks.nonEmpty) self ! SendBlocks
  }

  override def preStart(): Unit = logInfo(s"ModifiersHolder actor is started.")

  override def receiveRecover: Receive = if (settings.levelDb.enableRestore) receiveRecoverEnabled else receiveRecoverDisabled

  def receiveRecoverEnabled: Receive = {
    case header: EncryBlockHeader =>
      updateHeaders(header)
      logDebug(s"Header ${header.height} is recovered from leveldb.")
    case payload: EncryBlockPayload =>
      updatePayloads(payload)
      logDebug(s"Payload ${Algos.encode(payload.headerId)} is recovered from leveldb.")
    case block: EncryBlock =>
      updateCompletedBlocks(block)
      logDebug(s"Block ${block.header.height} is recovered from leveldb.")
    case RecoveryCompleted if completedBlocks.isEmpty =>
      logInfo("Recovery completed.")
      peerManager ! RecoveryCompleted
    case RecoveryCompleted =>
      context.system.scheduler.scheduleOnce(5 seconds)(self ! CheckAllBlocksSent)
  }

  def receiveRecoverDisabled: Receive = {
    case _ =>
  }

  override def receiveCommand: Receive = {
    case CheckAllBlocksSent =>
      if (completedBlocks.isEmpty) {
        logInfo("Recovery completed.")
        peerManager ! RecoveryCompleted
      }
      else context.system.scheduler.scheduleOnce(5 seconds)(self ! CheckAllBlocksSent)
    case SendBlocks =>
      val blocksToSend: Seq[EncryBlock] = completedBlocks.take(settings.levelDb.batchSize).values.toSeq
      completedBlocks = completedBlocks.drop(settings.levelDb.batchSize)
      nodeViewHolder ! BlocksFromLocalPersistence(blocksToSend)

    case RequestedModifiers(modifierTypeId, modifiers) => updateModifiers(modifierTypeId, modifiers)
    case lm: LocallyGeneratedModifier[EncryPersistentModifier] => updateModifiers(lm.pmod.modifierTypeId, Seq(lm.pmod))
    case x: Any => logError(s"Strange input: $x.")
  }

  def createBlockIfPossible(payloadId: ModifierId): Unit =
    nonCompletedBlocks.get(Algos.encode(payloadId)).foreach(headerId => headers.get(headerId).foreach { header =>
      payloads.get(Algos.encode(payloadId)).foreach { payload =>
        completedBlocks += header._1.height -> EncryBlock(header._1, payload._1, None)
        nonCompletedBlocks -= Algos.encode(payloadId)
      }
    })

  def updateModifiers(modsTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier]): Unit = modifiers.foreach {
    case header: EncryBlockHeader =>
      if (!headers.contains(Algos.encode(header.id)))
        persist(header) { header =>
          logDebug(s"Header at height: ${header.height} with id: ${Algos.encode(header.id)} is persisted successfully.")
        }
      updateHeaders(header)
      logDebug(s"Got header ${Algos.encode(header.id)} on height ${header.height}")
    case payload: EncryBlockPayload =>
      if (!payloads.contains(Algos.encode(payload.id)))
        persist(payload) { payload =>
          logDebug(s"Payload with id: ${Algos.encode(payload.id)} is persisted successfully.")
        }
      updatePayloads(payload)
      logDebug(s"Got payload with id: ${Algos.encode(payload.id)} " +
        s"${
          nonCompletedBlocks.get(Algos.encode(payload.id)).map(headerId =>
            headers.get(headerId).map(header => s"for header $headerId height: ${header._1.height}"))}")
    case block: EncryBlock =>
      if (!completedBlocks.values.toSeq.contains(block))
        persist(block) { block =>
          logDebug(s"Header at height: ${block.header.height} with id: ${Algos.encode(block.id)} is persisted successfully.")
        }
      updateCompletedBlocks(block)
    case x: Any => logError(s"Strange input $x.")
  }

  def updateHeaders(header: EncryBlockHeader): Unit = {
    val prevValue: (EncryBlockHeader, Int) = headers.getOrElse(Algos.encode(header.id), (header, -1))
    headers += Algos.encode(header.id) -> (prevValue._1, prevValue._2 + 1)
    if (!nonCompletedBlocks.contains(Algos.encode(header.payloadId)))
      nonCompletedBlocks += Algos.encode(header.payloadId) -> Algos.encode(header.id)
    else {
      nonCompletedBlocks = (nonCompletedBlocks - Algos.encode(header.payloadId)) +
        (Algos.encode(header.payloadId) -> Algos.encode(header.id))
      createBlockIfPossible(header.payloadId)
    }
  }

  def updatePayloads(payload: EncryBlockPayload): Unit = {
    val prevValue: (EncryBlockPayload, Int) = payloads.getOrElse(Algos.encode(payload.id), (payload, -1))
    payloads += Algos.encode(payload.id) -> (prevValue._1, prevValue._2 + 1)
    if (!nonCompletedBlocks.contains(Algos.encode(payload.id))) nonCompletedBlocks += Algos.encode(payload.id) -> ""
    else createBlockIfPossible(payload.id)
  }

  def updateCompletedBlocks(block: EncryBlock): Unit = completedBlocks += block.header.height -> block

  override def persistenceId: String = "persistent actor"

  override def journalPluginId: String = "akka.persistence.journal.leveldb"

  override def snapshotPluginId: String = "akka.persistence.snapshot-store.local"

}

object ModifiersHolder {

  case object SendBlocks

  case object CheckAllBlocksSent

  case class Statistics(receivedHeaders: Int,
                        receivedPayloads: Int,
                        notCompletedBlocks: Int,
                        completedBlocks: Int,
                        maxHeight: Int,
                        gaps: Seq[(Int, Int)],
                        duplicates: Seq[(ModifierId, Int)]) {
    override def toString: String = s"Stats: ${this.receivedHeaders} headers, " +
      s"${this.receivedPayloads} payloads, " +
      s"${this.notCompletedBlocks} incomplete blocks, " +
      s"${this.completedBlocks} full blocks, " +
      s"max height: ${this.maxHeight}, " +
      s"gaps: ${this.gaps.foldLeft("") { case (str, gap) => str + s"(${gap._1}, ${gap._2})" }} " +
      s"duplicates: ${
        this.duplicates.foldLeft("") { case (str, duplicate) =>
          str + s"(${Algos.encode(duplicate._1)}, ${duplicate._2})"
        }
      }."
  }

  case object Statistics {
    def apply(headers: Map[String, (EncryBlockHeader, Int)],
              payloads: Map[String, (EncryBlockPayload, Int)],
              nonCompletedBlocks: Map[String, String],
              completedBlocks: SortedMap[Int, EncryBlock]): Statistics =
      Statistics(
        headers.size,
        payloads.size,
        nonCompletedBlocks.size,
        completedBlocks.size,
        if (completedBlocks.nonEmpty) completedBlocks.keys.max else 0,
        countGaps(completedBlocks.keys.toSeq),
        headers.values.filter(_._2 > 1).map(headerWithDuplicatesQty =>
          headerWithDuplicatesQty._1.id -> headerWithDuplicatesQty._2).toSeq ++
          payloads.values.filter(_._2 > 1).map(payloadWithDuplicatesQty =>
            payloadWithDuplicatesQty._1.id -> payloadWithDuplicatesQty._2).toSeq
      )
  }

  case class RequestedModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier])

  def countGaps(heights: Seq[Int]): Seq[(Int, Int)] = heights.foldLeft(Seq[(Int, Int)](), -1) {
    case ((gaps, prevHeight), blockHeight) =>
      if (prevHeight + 1 != blockHeight) (gaps :+ (prevHeight + 1, blockHeight - 1), blockHeight)
      else (gaps, blockHeight)
  }._1
}