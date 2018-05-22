package encry.network

import akka.actor.{ActorRef, ActorRefFactory, Props}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proposition.EncryProposition
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.EncryMempool
import scorex.core.NodeViewHolder._
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SyntacticallySuccessfulModifier}
import scorex.core.network.message.BasicMsgDataTypes.ModifiersData
import scorex.core.network.message.{Message, ModifiersSpec}
import scorex.core.network.{NodeViewSynchronizer, SendToRandom}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, ModifierTypeId}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class EncryNodeViewSynchronizer(networkControllerRef: ActorRef,
                                viewHolderRef: ActorRef,
                                syncInfoSpec: EncrySyncInfoMessageSpec.type,
                                networkSettings: NetworkSettings,
                                timeProvider: NetworkTimeProvider)
  extends NodeViewSynchronizer[EncryProposition, EncryBaseTransaction,
    EncrySyncInfo, EncrySyncInfoMessageSpec.type, EncryPersistentModifier, EncryHistory,
    EncryMempool](networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider) {

  import EncryNodeViewSynchronizer._

  override protected val deliveryTracker =
    new EncryDeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, self, timeProvider)

  private val toDownloadCheckInterval = 3.seconds

  private val downloadListSize = networkSettings.networkChunkSize

  override def preStart(): Unit = {
    val toDownloadCheckInterval = networkSettings.syncInterval
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.scheduler.schedule(toDownloadCheckInterval, toDownloadCheckInterval)(self ! CheckModifiersToDownload)
  }

  protected val onSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(_: EncryBlock) =>
    //Do nothing, other nodes will request required modifiers via ProgressInfo.toDownload
    case SemanticallySuccessfulModifier(mod) =>
      broadcastModifierInv(mod)
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
  protected val onSyntacticallySuccessfulModifier: Receive = {
    case SyntacticallySuccessfulModifier(mod) if (mod.isInstanceOf[EncryBlockHeader] || mod.isInstanceOf[EncryBlockPayload]) &&
      historyReaderOpt.exists(_.isHeadersChainSynced) =>

      broadcastModifierInv(mod)
  }

  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      deliveryTracker.removeOutdatedExpectingFromRandom()
      historyReaderOpt.foreach { h =>
        val currentQueue = deliveryTracker.expectingFromRandomQueue
        val newIds = h.modifiersToDownload(downloadListSize - currentQueue.size, currentQueue)
        val oldIds = deliveryTracker.idsExpectingFromRandomToRetry()
        (newIds ++ oldIds).groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
  }

  private def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    modifierIds.foreach(id => deliveryTracker.expectFromRandom(modifierTypeId, id))
    val msg = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
    //todo: Full nodes should be here, not a random peer
    networkControllerRef ! SendToNetwork(msg, SendToRandom)
  }

  override protected def modifiersFromRemote: Receive = {
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote)
      if spec.messageCode == ModifiersSpec.messageCode =>
      super.modifiersFromRemote(DataFromPeer(spec, data, remote))
      //If queue is empty - check, whether there are more modifiers to download
      historyReaderOpt foreach { h =>
        if (!h.isHeadersChainSynced && !deliveryTracker.isExpecting) {
          // headers chain is not synced yet, but our expecting list is empty - ask for more headers
          sendSync(h.syncInfo)
        } else if (h.isHeadersChainSynced && !deliveryTracker.isExpectingFromRandom) {
          // headers chain is synced, but our full block list is empty - request more full blocks
          self ! CheckModifiersToDownload
        }
      }
  }
}

object EncryNodeViewSynchronizer {

  case object CheckModifiersToDownload

  def props(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: EncrySyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider): Props =
    Props(new EncryNodeViewSynchronizer(networkControllerRef, viewHolderRef,
      syncInfoSpec, networkSettings, timeProvider))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: EncrySyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef,
      syncInfoSpec, networkSettings, timeProvider))
}
