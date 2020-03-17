package encry.nvg

import akka.actor.{ Actor, Cancellable, Props }
import cats.syntax.option._
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.{ HistoryComparisonResult, Younger }
import encry.network.DeliveryManager.CheckPayloadsToDownload
import encry.network.Messages.MessageToNetwork.{ BroadcastModifier, RequestFromLocal, ResponseFromLocal, SendSyncInfo }
import encry.network.ModifiersToNetworkUtils.toProto
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.OtherNodeSyncingStatus
import encry.nvg.NodeViewHolder.{ SemanticallySuccessfulModifier, UpdateHistoryReader }
import encry.settings.EncryAppSettings
import encry.utils.Utils.idsToString
import encry.view.history.HistoryReader
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ Block, Header, Payload }
import org.encryfoundation.common.network.BasicMessagesRepo.{
  InvNetworkMessage,
  RequestModifiersNetworkMessage,
  SyncInfoNetworkMessage
}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.concurrent.duration._

class NetworkMessagesProcessor(settings: EncryAppSettings) extends Actor with StrictLogging {

  import context.dispatcher

  var historyReader: HistoryReader = HistoryReader.empty

  var modifiersRequestCache: Map[String, Array[Byte]] = Map.empty

  context.system.scheduler.schedule(5.seconds, settings.network.syncInterval) {
    logger.debug("Scheduler once for SendLocalSyncInfo triggered")
    context.parent ! SendSyncInfo(historyReader.syncInfo)
  }

  override def receive(): Receive =
    workingCycle(
      context.system.scheduler
        .scheduleOnce(settings.network.modifierDeliverTimeCheck)(self ! CheckPayloadsToDownload)
        .some
    )

  def workingCycle(modifiersRequester: Option[Cancellable]): Receive = {
    case UpdateHistoryReader(newReader: HistoryReader) =>
      historyReader = newReader
    case SemanticallySuccessfulModifier(mod) =>
      //todo possible way to call CheckPayloadsToDownload
      mod match {
        case block: Block if historyReader.isFullChainSynced =>
          List(block.header, block.payload).foreach { mod: PersistentModifier =>
            logger.info(s"Going to broadcast inv for modifier of type ${mod.modifierTypeId} with id: ${mod.encodedId}.")
            context.parent ! BroadcastModifier(mod.modifierTypeId, mod.id)
          }
          modifiersRequestCache = Map(
            block.encodedId         -> toProto(block.header),
            block.payload.encodedId -> toProto(block.payload)
          )
        case _ =>
      }
    case DataFromPeer(message, remote) =>
      message match {
        case SyncInfoNetworkMessage(syncInfo: SyncInfo) =>
          logger.info(s"Got sync info network message. This message includes ${syncInfo.lastHeaderIds.size} ids.")
          val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
          logger.info(
            s"\n\n Comparison with $remote has starting points ${idsToString(syncInfo.startingPoints)}.\n " +
              s"Comparison result is $comparison. \n "
          )
          context.parent ! OtherNodeSyncingStatus(remote, comparison)

        case InvNetworkMessage(invData) if invData._1 == Payload.modifierTypeId && !historyReader.isFullChainSynced =>
          logger.info(
            s"Got inv message with payloads: ${invData._2.map(Algos.encode).mkString(",")}. " +
              s"But full chain is not synced. Ignore them."
          )

        case InvNetworkMessage(invData) =>
          val startTime: Long = System.currentTimeMillis()
          val ids: Seq[ModifierId] = invData._2.filterNot(
            (mid: ModifierId) =>
              historyReader.isModifierDefined(mid) || ModifiersCache.contains(NodeViewHolder.toKey(mid))
          )
          logger.info(
            s"Got inv message from $remote. Type of nested ids is: ${invData._1}. " +
              s"Ids for request are: ${ids.map(Algos.encode).mkString(",")}."
          )
          if (ids.nonEmpty &&
              (invData._1 == Header.modifierTypeId ||
              (historyReader.isHeadersChainSyncedVar && invData._1 == Payload.modifierTypeId)))
            sender() ! RequestFromLocal(remote.some, invData._1, ids.toList)
          logger.info(s"Time of processing inv message is: ${(System.currentTimeMillis() - startTime) / 1000}s.")

        case RequestModifiersNetworkMessage((typeId, requestedIds)) =>
          val modifiersFromCache: Map[ModifierId, Array[Byte]] = requestedIds
            .flatMap(
              (id: ModifierId) =>
                modifiersRequestCache
                  .get(Algos.encode(id))
                  .map(id -> _)
            )
            .toMap
          if (modifiersFromCache.nonEmpty) context.parent ! ResponseFromLocal(remote, typeId, modifiersFromCache)
          val unrequestedModifiers: List[ModifierId] = requestedIds.filterNot(modifiersFromCache.contains).toList

          typeId match {
            case h if h == Header.modifierTypeId =>
              context.parent ! ResponseFromLocal(remote, typeId, getModsForRemote(unrequestedModifiers, historyReader))
            case _ =>
              getModsForRemote(unrequestedModifiers, historyReader).foreach {
                case (id: ModifierId, bytes: Array[Byte]) =>
                  context.parent ! ResponseFromLocal(remote, typeId, Map(id -> bytes))
              }
          }
      }
    case CheckPayloadsToDownload =>
      val newIds: Seq[ModifierId] = historyReader.payloadsIdsToDownload(settings.network.networkChunkSize)
      logger.debug(s"newIds: ${newIds.map(elem => Algos.encode(elem)).mkString(",")}")
      if (newIds.nonEmpty) context.parent ! RequestFromLocal(none, Payload.modifierTypeId, newIds.toList)
      val nextCheckModsScheduler: Cancellable =
        context.system.scheduler.scheduleOnce(settings.network.modifierDeliverTimeCheck)(self ! CheckPayloadsToDownload)
      context.become(workingCycle(nextCheckModsScheduler.some))
  }

  def getModsForRemote(ids: List[ModifierId], reader: HistoryReader): Map[ModifierId, Array[Byte]] =
    ids.view
      .map(id => id -> reader.modifierBytesById(id))
      .collect { case (id, mod) if mod.isDefined => id -> mod.get }
      .toMap
}

object NetworkMessagesProcessor {
  def props(settings: EncryAppSettings): Props = Props(new NetworkMessagesProcessor(settings))
}
