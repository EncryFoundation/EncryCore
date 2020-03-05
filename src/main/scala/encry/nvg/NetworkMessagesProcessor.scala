package encry.nvg

import akka.actor.{ Actor, Props }
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.{ HistoryComparisonResult, Younger }
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.OtherNodeSyncingStatus
import encry.settings.EncryAppSettings
import encry.utils.Utils.idsToString
import encry.view.ModifiersCache
import encry.view.history.HistoryReader
import org.encryfoundation.common.modifiers.history.{ Header, Payload }
import org.encryfoundation.common.network.BasicMessagesRepo.{
  InvNetworkMessage,
  ModifiersNetworkMessage,
  RequestModifiersNetworkMessage,
  SyncInfoNetworkMessage
}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.Seq

class NetworkMessagesProcessor(settings: EncryAppSettings) extends Actor with StrictLogging {

  var historyReader: HistoryReader = HistoryReader.empty

  //todo last modifiers cache?

  override def receive: Receive = {
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
          getModsForRemote(requestedIds.toList, historyReader).foreach {
            case (id: ModifierId, bytes: Array[Byte]) =>
              context.parent ! ModifiersNetworkMessage(typeId -> Map(id -> bytes))
          }
        case RequestModifiersNetworkMessage((typeId, requestedIds)) =>
          context.parent ! ModifiersNetworkMessage(typeId -> getModsForRemote(requestedIds.toList, historyReader))
      }
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
