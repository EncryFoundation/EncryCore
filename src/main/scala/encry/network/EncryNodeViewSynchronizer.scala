package encry.network

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proposition.EncryProposition
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.EncryMempool
import encry.EncryApp._
import encry.view.NodeViewHolder._
import encry.network.NetworkController.ReceivableMessages.SendToNetwork
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SyntacticallySuccessfulModifier}
import encry.network.message.BasicMsgDataTypes.ModifiersData
import encry.network.message.{Message, ModifiersSpec}
import encry.settings.NetworkSettings
import scorex.core.{ModifierId, ModifierTypeId}
import scala.concurrent.duration._

class EncryNodeViewSynchronizer(syncInfoSpec: EncrySyncInfoMessageSpec.type)
  extends NodeViewSynchronizer[EncryProposition, EncryBaseTransaction,
    EncrySyncInfo, EncrySyncInfoMessageSpec.type, EncryPersistentModifier, EncryHistory,
    EncryMempool](networkController, nodeViewHolder, syncInfoSpec, settings.network, timeProvider) {

  import EncryNodeViewSynchronizer._

  override protected val deliveryTracker = EncryDeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, self, timeProvider)

  val networkSettings: NetworkSettings = settings.network

  val toDownloadCheckInterval: FiniteDuration = 3.seconds

  val downloadListSize: Int = networkSettings.networkChunkSize

  val onSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(_: EncryBlock) =>
    //Do nothing, other nodes will request required modifiers via ProgressInfo.toDownload
    case SemanticallySuccessfulModifier(mod) => broadcastModifierInv(mod)
  }

  override def preStart(): Unit = {
    val toDownloadCheckInterval = networkSettings.syncInterval
    super.preStart()
    context.system.scheduler.schedule(toDownloadCheckInterval, toDownloadCheckInterval)(self ! CheckModifiersToDownload)
  }

  override protected def viewHolderEvents: Receive =
    onSyntacticallySuccessfulModifier orElse
      onDownloadRequest orElse
      onCheckModifiersToDownload orElse
      super.viewHolderEvents

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      requestDownload(modifierTypeId, Seq(modifierId))
  }

  /**
    * Broadcast inv on successful Header and BlockPayload application
    * Do not broadcast Inv messages during initial synchronization (the rest of the network should already have all
    * this messages)
    *
    */
  def onSyntacticallySuccessfulModifier: Receive = {
    case SyntacticallySuccessfulModifier(mod) if (mod.isInstanceOf[EncryBlockHeader] || mod.isInstanceOf[EncryBlockPayload]) &&
      historyReaderOpt.exists(_.isHeadersChainSynced) => broadcastModifierInv(mod)
  }

  def onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      deliveryTracker.removeOutdatedExpectingFromRandom()
      historyReaderOpt.foreach { h =>
        val currentQueue: Iterable[ModifierId] = deliveryTracker.expectingFromRandomQueue
        val newIds: Seq[(ModifierTypeId, ModifierId)] = h.modifiersToDownload(downloadListSize - currentQueue.size, currentQueue)
        val oldIds: Seq[(ModifierTypeId, ModifierId)] = deliveryTracker.idsExpectingFromRandomToRetry()
        (newIds ++ oldIds).groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
  }

  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    modifierIds.foreach(id => deliveryTracker.expectFromRandom(modifierTypeId, id))
    val msg: Message[(ModifierTypeId, Seq[ModifierId])] = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
    //todo: Full nodes should be here, not a random peer
    networkController ! SendToNetwork(msg, SendToRandom)
  }

  override protected def modifiersFromRemote: Receive = {
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      super.modifiersFromRemote(DataFromPeer(spec, data, remote))
      //If queue is empty - check, whether there are more modifiers to download
      historyReaderOpt foreach { h =>
        if (!h.isHeadersChainSynced && !deliveryTracker.isExpecting) sendSync(h.syncInfo)
        else if (h.isHeadersChainSynced && !deliveryTracker.isExpectingFromRandom) self ! CheckModifiersToDownload
      }
  }
}

object EncryNodeViewSynchronizer {

  case object CheckModifiersToDownload

}
