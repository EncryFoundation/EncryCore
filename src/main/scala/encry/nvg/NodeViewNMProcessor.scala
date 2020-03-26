package encry.nvg

import akka.actor.{ Actor, Cancellable, Props }
import cats.syntax.option._
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.HistoryComparisonResult
import encry.network.DeliveryManager.CheckPayloadsToDownload
import encry.network.Messages.MessageToNetwork.{
  BroadcastModifier,
  NotifyNodeAboutModifier,
  RequestFromLocal,
  ResponseFromLocal,
  SendSyncInfo
}
import encry.network.ModifiersToNetworkUtils.toProto
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.OtherNodeSyncingStatus
import encry.nvg.NodeViewHolder.{ SemanticallySuccessfulModifier, UpdateHistoryReader }
import encry.settings.EncryAppSettings
import encry.utils.Utils.idsToString
import encry.view.history.HistoryReader
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ Block, Header, Payload }
import org.encryfoundation.common.network.BasicMessagesRepo.BasicMsgDataTypes.InvData
import org.encryfoundation.common.network.BasicMessagesRepo.{
  InvNetworkMessage,
  RequestModifiersNetworkMessage,
  SyncInfoNetworkMessage
}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.concurrent.duration._

class NodeViewNMProcessor(settings: EncryAppSettings) extends Actor with StrictLogging {

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
    case msg: HistoryReader => historyReader = msg

    //todo possible way to call CheckPayloadsToDownload
    case SemanticallySuccessfulModifier(block: Block) if historyReader.isFullChainSynced =>
      List(block.header, block.payload).foreach { mod: PersistentModifier =>
        logger.info(s"Going to broadcast inv for modifier of type ${mod.modifierTypeId} with id: ${mod.encodedId}.")
        context.parent ! BroadcastModifier(mod.modifierTypeId, mod.id)
      }
      modifiersRequestCache = Map(
        block.encodedId         -> toProto(block.header),
        block.payload.encodedId -> toProto(block.payload)
      )

    case SemanticallySuccessfulModifier(_) =>
    case DataFromPeer(SyncInfoNetworkMessage(syncInfo: SyncInfo), remote) =>
      val extension: Seq[ModifierId] =
        historyReader.continuationIds(syncInfo, settings.network.syncPacketLength)
      val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
      logger.info(
        s"\n\nComparison with $remote has starting points ${idsToString(syncInfo.startingPoints)}.\n" +
          s"Comparison result is $comparison. Extension length is: ${extension.size}.\n "
      )
      context.parent ! OtherNodeSyncingStatus(remote, comparison)
      context.parent ! NotifyNodeAboutModifier(remote, Header.modifierTypeId, extension.toList)

    case DataFromPeer(InvNetworkMessage(invData: InvData), remote) =>
      if (invData._1 == Payload.modifierTypeId && !historyReader.isFullChainSynced)
        logger.info(s"Got inv message from $remote with ${invData._2.size} ids but full chain is not synced.")
      else {
        val isHeader: Boolean = invData._1 == Header.modifierTypeId
        val isPayloadAvailable: Boolean =
          historyReader.isHeadersChainSyncedVar && invData._1 == Payload.modifierTypeId
        val isRequestAvailable: Boolean = isHeader || isPayloadAvailable
        if (isRequestAvailable) {
          val ids: Seq[ModifierId] = invData._2.filterNot { mid: ModifierId =>
            historyReader.isModifierDefined(mid) || ModifiersCache.contains(NodeViewHolder.toKey(mid))
          }
          logger.info(s"Sending request from local to $remote with ${ids.size} ids of type ${invData._1}.")
          if (ids.nonEmpty) context.parent ! RequestFromLocal(remote.some, invData._1, ids.toList)
        } else
          logger.info(
            s"Got inv message from $remote but response is unavailable cause:" +
              s" is header - $isHeader || is payload available - $isPayloadAvailable."
          )
      }

    case DataFromPeer(RequestModifiersNetworkMessage((typeId, requestedIds)), remote) =>
      val modifiersFromCache: Map[ModifierId, Array[Byte]] = requestedIds.flatMap { id: ModifierId =>
        modifiersRequestCache
          .get(Algos.encode(id))
          .map(id -> _)
      }.toMap
      if (modifiersFromCache.nonEmpty) {
        logger.info(
          s"Send response from local with mods from cache: " +
            s" ${modifiersFromCache.keys.toList.map(Algos.encode).mkString(",")} to $remote."
        )
        context.parent ! ResponseFromLocal(remote, typeId, modifiersFromCache)
      }
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

    case CheckPayloadsToDownload =>
      val newIds: Seq[ModifierId] = historyReader.payloadsIdsToDownload(settings.network.networkChunkSize)
      logger.info(s"newIds: ${newIds.map(Algos.encode).mkString(",")}")
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

object NodeViewNMProcessor {
  def props(settings: EncryAppSettings): Props = Props(new NodeViewNMProcessor(settings))
}
