package encry.nvg

import akka.actor.{Actor, Cancellable, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.{HistoryComparisonResult, Younger}
import encry.network.Broadcast
import cats.syntax.option._
import encry.network.DeliveryManager.CheckPayloadsToDownload
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.ModifiersToNetworkUtils.toProto
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.{OtherNodeSyncingStatus, SemanticallySuccessfulModifier}
import encry.network.PeersKeeper.SendToNetwork
import encry.nvg.NetworkMessagesProcessor.IdsForRequest
import encry.settings.EncryAppSettings
import encry.utils.Utils.idsToString
import encry.view.history.HistoryReader
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.network.BasicMessagesRepo.{InvNetworkMessage, ModifiersNetworkMessage, RequestModifiersNetworkMessage, SyncInfoNetworkMessage}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

class NetworkMessagesProcessor(settings: EncryAppSettings) extends Actor with StrictLogging {

  import context.dispatcher

  var historyReader: HistoryReader = HistoryReader.empty

  var modifiersRequestCache: Map[String, Array[Byte]] = Map.empty

  override def receive(): Receive =
    workingCycle(
      context.system.scheduler
        .scheduleOnce(settings.network.modifierDeliverTimeCheck)(self ! CheckPayloadsToDownload)
        .some
    )

  def workingCycle(modifiersRequester: Option[Cancellable]): Receive = {
    case SemanticallySuccessfulModifier(mod) =>
      //todo possible way to call CheckPayloadsToDownload
      mod match {
        case block: Block if historyReader.isFullChainSynced =>
          List(block.header, block.payload).foreach { mod: PersistentModifier =>
            logger.info(s"Going to broadcast inv for modifier of type ${mod.modifierTypeId} with id: ${mod.encodedId}.")
            context.parent ! SendToNetwork(InvNetworkMessage(mod.modifierTypeId -> Seq(mod.id)), Broadcast)
          }
          modifiersRequestCache = Map(
            block.encodedId         -> toProto(block.header),
            block.payload.encodedId -> toProto(block.payload)
          )
      }
    case DataFromPeer(message, remote) =>
      message match {
        case SyncInfoNetworkMessage(syncInfo: SyncInfo) =>
          val ext: Seq[ModifierId] =
            historyReader.continuationIds(syncInfo, settings.network.syncPacketLength)
          val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
          logger.info(
            s"Comparison with $remote has starting points ${idsToString(syncInfo.startingPoints)}. " +
              s"Comparison result is $comparison. Sending extension of length ${ext.length}."
          )
          if (!(ext.nonEmpty || comparison != Younger)) logger.info("Extension is empty while comparison is younger")
          context.parent ! OtherNodeSyncingStatus(remote, comparison, Some(ext.map(h => Header.modifierTypeId -> h)))

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
            sender() ! RequestFromLocal(remote, invData._1, ids.toList)
          logger.info(s"Time of processing inv message is: ${(System.currentTimeMillis() - startTime) / 1000}s.")

        case RequestModifiersNetworkMessage((typeId, requestedIds)) if typeId == Payload.modifierTypeId =>
          val modifiersFromCache: Map[ModifierId, Array[Byte]] = requestedIds
            .flatMap(
              (id: ModifierId) =>
                modifiersRequestCache
                  .get(Algos.encode(id))
                  .map(id -> _)
            )
            .toMap
          if (modifiersFromCache.nonEmpty) context.parent ! ModifiersNetworkMessage(typeId -> modifiersFromCache)
          val unrequestedModifiers: List[ModifierId] = requestedIds.filterNot(modifiersFromCache.contains).toList

          typeId match {
            case h if h == Header.modifierTypeId =>
              context.parent ! ModifiersNetworkMessage(typeId -> getModsForRemote(unrequestedModifiers, historyReader))
            case _ =>
              getModsForRemote(unrequestedModifiers, historyReader).foreach {
                case (id: ModifierId, bytes: Array[Byte]) =>
                  context.parent ! ModifiersNetworkMessage(typeId -> Map(id -> bytes))
              }
          }
      }
    case CheckPayloadsToDownload =>
      val newIds: Seq[ModifierId] = historyReader.payloadsIdsToDownload(settings.network.networkChunkSize)
      logger.debug(s"newIds: ${newIds.map(elem => Algos.encode(elem)).mkString(",")}")
      if (newIds.nonEmpty) context.parent ! IdsForRequest(newIds.toList)
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
  final case class IdsForRequest(ids: List[ModifierId]) extends AnyVal
  def props(settings: EncryAppSettings): Props = Props(new NetworkMessagesProcessor(settings))
}
