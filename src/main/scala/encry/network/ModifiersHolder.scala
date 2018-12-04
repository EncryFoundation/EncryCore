package encry.network

import akka.actor.ActorRef
import akka.persistence._
import com.typesafe.scalalogging.StrictLogging
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.EncryApp._
import encry.modifiers.history.{Block, Header, Payload}
import encry.modifiers.{EncryPersistentModifier, NodeViewModifier}
import encry.network.ModifiersHolder._
import encry.settings.EncryAppSettings
import encry.view.EncryNodeViewHolder.ReceivableMessages.{BlocksFromLocalPersistence, LocallyGeneratedModifier}
import org.encryfoundation.common.Algos
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._
import scala.language.postfixOps

class ModifiersHolder(settings: EncryAppSettings, postgresRestoreOpt: Option[ActorRef]) extends PersistentActor with StrictLogging {

  var stat: Statistics = Statistics(0, 0, 0, 0, 0, Seq.empty, Seq.empty)
  var headers: Map[String, (Header, Int)] = Map.empty
  var payloads: Map[String, (Payload, Int)] = Map.empty
  var nonCompletedBlocks: Map[String, String] = Map.empty
  var completedBlocks: SortedMap[Int, Block] = SortedMap.empty

  context.system.scheduler.schedule(10.second, 30.second) {
    logger.debug(Statistics(headers, payloads, nonCompletedBlocks, completedBlocks).toString)
  }

  context.system.scheduler.scheduleOnce(5 seconds) {
    if (completedBlocks.nonEmpty) self ! SendBlocks
  }

  def notifyRecoveryCompleted(): Unit = if (settings.postgres.exists(_.enableRestore)) {
    logger.info("Recovery from levelDb completed, going to download rest of the blocks from postgres")
    postgresRestoreOpt.foreach(_ ! StartRecovery)
  } else {
    logger.info("Recovery completed")
    peerManager ! RecoveryCompleted
  }

  override def preStart(): Unit = logger.info(s"ModifiersHolder actor is started.")

  override def receiveRecover: Receive =
    if (settings.levelDb.exists(_.enableRestore)) receiveRecoverEnabled else receiveRecoverDisabled

  def receiveRecoverEnabled: Receive = {
    case header: Header => updateHeaders(header)
    case payload: Payload => updatePayloads(payload)
    case block: Block => updateCompletedBlocks(block)
    case RecoveryCompleted if completedBlocks.isEmpty => notifyRecoveryCompleted()
    case RecoveryCompleted => context.system.scheduler.scheduleOnce(5 seconds)(self ! CheckAllBlocksSent)
  }

  def receiveRecoverDisabled: Receive = {
    case _ =>
  }

  override def receiveCommand: Receive = {
    case CheckAllBlocksSent =>
      if (completedBlocks.isEmpty) notifyRecoveryCompleted()
      else context.system.scheduler.scheduleOnce(5 seconds)(self ! CheckAllBlocksSent)
    case SendBlocks =>
      val blocksToSend: Seq[Block] = completedBlocks.take(settings.levelDb
        .map(_.batchSize)
        .getOrElse(throw new RuntimeException("batchsize not specified"))).values.toSeq

      completedBlocks = completedBlocks.drop(settings.levelDb
        .map(_.batchSize)
        .getOrElse(throw new RuntimeException("batchsize not specified")))
      context.parent ! BlocksFromLocalPersistence(blocksToSend, completedBlocks.isEmpty &&
        !settings.postgres.exists(_.enableRestore))

    case RequestedModifiers(modifierTypeId, modifiers) => updateModifiers(modifierTypeId, modifiers)
    case lm: LocallyGeneratedModifier[EncryPersistentModifier] => updateModifiers(lm.pmod.modifierTypeId, Seq(lm.pmod))
    case x: Any => logger.error(s"Strange input: $x.")
  }

  def createBlockIfPossible(payloadId: ModifierId): Unit =
    nonCompletedBlocks.get(Algos.encode(payloadId)).foreach(headerId => headers.get(headerId).foreach { header =>
      payloads.get(Algos.encode(payloadId)).foreach { payload =>
        completedBlocks += header._1.height -> Block(header._1, payload._1, None)
        nonCompletedBlocks -= Algos.encode(payloadId)
      }
    })

  def updateModifiers(modsTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier]): Unit = modifiers.foreach {
    case header: Header =>
      if (!headers.contains(Algos.encode(header.id)))
        persist(header) { header =>
          logger.debug(s"Header at height: ${header.height} with id: ${Algos.encode(header.id)} " +
            s"is persisted successfully.")
        }
      updateHeaders(header)
    case payload: Payload =>
      if (!payloads.contains(Algos.encode(payload.id)))
        persist(payload) { payload =>
          logger.debug(s"Payload with id: ${Algos.encode(payload.id)} is persisted successfully.")
        }
      updatePayloads(payload)
    case block: Block =>
      if (!completedBlocks.values.toSeq.contains(block))
        persist(block) { block =>
          logger.debug(s"Header at height: ${block.header.height} with id: ${Algos.encode(block.id)} " +
            s"is persisted successfully.")
        }
      updateCompletedBlocks(block)
    case x: Any => logger.error(s"Strange input $x.")
  }

  def updateHeaders(header: Header): Unit = {
    val prevValue: (Header, Int) = headers.getOrElse(Algos.encode(header.id), (header, -1))
    headers += Algos.encode(header.id) -> (prevValue._1, prevValue._2 + 1)
    if (!nonCompletedBlocks.contains(Algos.encode(header.payloadId)))
      nonCompletedBlocks += Algos.encode(header.payloadId) -> Algos.encode(header.id)
    else {
      nonCompletedBlocks = (nonCompletedBlocks - Algos.encode(header.payloadId)) +
        (Algos.encode(header.payloadId) -> Algos.encode(header.id))
      createBlockIfPossible(header.payloadId)
    }
  }

  def updatePayloads(payload: Payload): Unit = {
    val prevValue: (Payload, Int) = payloads.getOrElse(Algos.encode(payload.id), (payload, -1))
    payloads += Algos.encode(payload.id) -> (prevValue._1, prevValue._2 + 1)
    if (!nonCompletedBlocks.contains(Algos.encode(payload.id))) nonCompletedBlocks += Algos.encode(payload.id) -> ""
    else createBlockIfPossible(payload.id)
  }

  def updateCompletedBlocks(block: Block): Unit = completedBlocks += block.header.height -> block

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
    def apply(headers: Map[String, (Header, Int)],
              payloads: Map[String, (Payload, Int)],
              nonCompletedBlocks: Map[String, String],
              completedBlocks: SortedMap[Int, Block]): Statistics =
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