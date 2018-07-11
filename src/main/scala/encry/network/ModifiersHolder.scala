package encry.network

import akka.persistence._
import encry.EncryApp._
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.{EncryPersistentModifier, NodeViewModifier}
import encry.network.ModifiersHolder._
import encry.settings.Algos
import encry.utils.Logging
import encry.view.EncryNodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import encry.{ModifierId, ModifierTypeId}

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

class ModifiersHolder extends PersistentActor with Logging {

  var stat: Statistics = Statistics(0, 0, 0, 0, 0, Seq.empty, Seq.empty)

  /** Map, which contains not completed blocks
    * Key can be payloadId or also header id. So value depends on key, and can contains: headerId, payloadId */
  var headers: Map[String, (EncryBlockHeader, Int)] = Map.empty
  var payloads: Map[String, (EncryBlockPayload, Int)] = Map.empty
  var nonCompletedBlocks: Map[String, String] = Map.empty
  var completedBlocks: SortedMap[Int, EncryBlock] = SortedMap.empty

  context.system.scheduler.schedule(10.second, 30.second) {
    stat = Statistics(
      headers.size,
      payloads.size,
      nonCompletedBlocks.size,
      completedBlocks.size,
      if (completedBlocks.nonEmpty) completedBlocks.keys.max else 0,
      countGaps(completedBlocks.keys.toSeq),
      headers.values.filter(_._2 > 1).map(headerWithDuplicatesQty => headerWithDuplicatesQty._1.id -> headerWithDuplicatesQty._2).toSeq ++
        payloads.values.filter(_._2 > 1).map(payloadWithDuplicatesQty => payloadWithDuplicatesQty._1.id -> payloadWithDuplicatesQty._2).toSeq
    )
    persist(stat) { pieceOfStat => logger.info(pieceOfStat.toString) }
  }

  override def preStart(): Unit = logger.info(s"ModifiersHolder actor is started.")

  override def receiveRecover: Receive = {
    case SnapshotOffer(_, snapshotAboutStat: Statistics) => stat = snapshotAboutStat
    case header: EncryBlockHeader =>
      updateHeaders(header)
      logger.debug(s"Header ${header.height} is recovered from leveldb")
    case payload: EncryBlockPayload =>
      updatePayloads(payload)
      logger.debug(s"Payload ${Algos.encode(payload.headerId)} is recovered from leveldb")
    case block: EncryBlock =>
      updateCompletedBlocks(block)
      logger.debug(s"Block ${block.header.height} is recovered from leveldb")
  }

  override def receiveCommand: Receive = {
    case SaveSnapshotSuccess(_) => logger.info("Success with snapshot save.")
    case SaveSnapshotFailure(_, _) => logger.info("Failure with snapshot save.")
    case RecoverState =>
      logger.info("Starting to recover state from Modifiers Holder")
      val sortedHeaders: Seq[EncryBlockHeader] = headers.map(_._2._1).toSeq
        .sortWith((firstHeader, secondHeader) => firstHeader.height < secondHeader.height)
      if (sortedHeaders.headOption.exists(_.height == 0)) sortedHeaders.tail.foldLeft(Seq(sortedHeaders.head)) {
        case (applicableHeaders, header) =>
          if (applicableHeaders.last.height + 1 == header.height) applicableHeaders :+ header
          else applicableHeaders
      }.foreach(header => nodeViewHolder ! LocallyGeneratedModifier(header))
      if (completedBlocks.keys.headOption.contains(0)) completedBlocks.foldLeft(Seq(completedBlocks.head._2)) {
        case (applicableBlocks, blockWithHeight) =>
          if (applicableBlocks.last.header.height + 1 == blockWithHeight._1) applicableBlocks :+ blockWithHeight._2
          else applicableBlocks
      }.foreach(block => nodeViewHolder ! LocallyGeneratedModifier(block.payload))
    case RequestedModifiers(modifierTypeId, modifiers) => updateModifiers(modifierTypeId, modifiers)
    case lm: LocallyGeneratedModifier[EncryPersistentModifier] => updateModifiers(lm.pmod.modifierTypeId, Seq(lm.pmod))
    case x: Any => logger.info(s"Strange input: $x")
  }

  def createBlockIfPossible(payloadId: ModifierId): Unit =
    nonCompletedBlocks.get(Algos.encode(payloadId)).foreach(headerId => headers.get(headerId).foreach { header =>
      payloads.get(Algos.encode(payloadId)).foreach { payload =>
        completedBlocks += header._1.height -> EncryBlock(header._1, payload._1, None)
        nonCompletedBlocks -= Algos.encode(payloadId)
      }
    })

  def updateModifiers(modsTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier]): Unit = modifiers.foreach {
    case header: EncryBlockHeader => persist(header) { header => updateHeaders(header) }
    case payload: EncryBlockPayload => persist(payload) { payload => updatePayloads(payload) }
    case block: EncryBlock => persist(block) { block => updateCompletedBlocks(block) }
    case _ =>
  }

  def updateHeaders(header: EncryBlockHeader): Unit = {
    val prevValue: (EncryBlockHeader, Int) = headers.getOrElse(Algos.encode(header.id), (header, -1))
    headers += Algos.encode(header.id) -> (prevValue._1, prevValue._2 + 1)
    if (!nonCompletedBlocks.contains(Algos.encode(header.payloadId)))
      nonCompletedBlocks += Algos.encode(header.payloadId) -> Algos.encode(header.id)
    else {
      nonCompletedBlocks = (nonCompletedBlocks - Algos.encode(header.payloadId)) + (Algos.encode(header.payloadId) -> Algos.encode(header.id))
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

  case object RecoverState

  case class Statistics(receivedHeaders: Int,
                        receivedPayloads: Int,
                        notCompletedBlocks: Int,
                        completedBlocks: Int,
                        maxHeight: Int,
                        gaps: Seq[(Int, Int)],
                        duplicates: Seq[(ModifierId, Int)]) {
    override def toString: String = s"Stats: ${this.receivedHeaders} headers received - " +
      s"${this.receivedPayloads} payloads received - " +
      s"${this.notCompletedBlocks} not full blocks - " +
      s"${this.completedBlocks} full blocks - " +
      s"max height: ${this.maxHeight} " +
      s"Gaps: ${this.gaps.foldLeft("") { case (str, gap) => str + s"(${gap._1}, ${gap._2})" }} " +
      s"Duplicates: ${this.duplicates.foldLeft("") { case (str, duplicate) => str + s"(${Algos.encode(duplicate._1)}, ${duplicate._2})" }}"
  }

  case class RequestedModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier])

  def countGaps(heights: Seq[Int]): Seq[(Int, Int)] = heights.foldLeft(Seq[(Int, Int)](), -1) {
    case ((gaps, prevHeight), blockHeight) =>
      if (prevHeight + 1 != blockHeight) (gaps :+ (prevHeight + 1, blockHeight - 1), blockHeight)
      else (gaps, blockHeight)
  }._1
}