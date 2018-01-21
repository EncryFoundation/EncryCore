package encry.network

import akka.actor.ActorRef
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.EncryBaseTransaction
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder._
import scorex.core.network.NetworkController.SendToNetwork
import scorex.core.network.message.Message
import scorex.core.network.{NodeViewSynchronizer, SendToRandom}
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, ModifierTypeId, NodeViewHolder}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class EncryNodeViewSynchronizer(networkControllerRef: ActorRef,
                                viewHolderRef: ActorRef,
                                localInterfaceRef: ActorRef,
                                syncInfoSpec: EncrySyncInfoMessageSpec.type,
                                networkSettings: NetworkSettings,
                                timeProvider: NetworkTimeProvider)
  extends NodeViewSynchronizer[Proposition, EncryBaseTransaction,
    EncrySyncInfo, EncrySyncInfoMessageSpec.type, EncryPersistentModifier, EncryHistory,
    EncryMempool](networkControllerRef, viewHolderRef, localInterfaceRef,
    syncInfoSpec, networkSettings, timeProvider) {

  import EncryNodeViewSynchronizer._

  override protected val deliveryTracker =
    new EncryDeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, self, timeProvider)

  private val toDownloadCheckInterval = 3.seconds

  override def preStart(): Unit = {
    viewHolderRef ! Subscribe(Seq(NodeViewHolder.EventType.DownloadNeeded))
    super.preStart()
    context.system.scheduler.schedule(toDownloadCheckInterval, toDownloadCheckInterval)(self ! CheckModifiersToDownload)
    initializeToDownload()
  }

  protected def initializeToDownload(): Unit = {
    viewHolderRef ! GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, MissedModifiers] { v =>
      MissedModifiers(v.history.missedModifiersForFullChain())
    }
  }

  protected def onMissedModifiers(): Receive = {
    case MissedModifiers(ids) =>
      log.info(s"Initialize toDownload with ${ids.length} ids: ${scorex.core.idsToString(ids)}")
      ids.foreach(id => requestDownload(id._1, id._2))
  }

  protected val onSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(_: EncryBlock) =>
    // Do nothing, other nodes will request required modifiers via ProgressInfo.toDownload
    case SemanticallySuccessfulModifier(mod) =>
      broadcastModifierInv(mod)
  }

  override protected def viewHolderEvents: Receive = onSemanticallySuccessfulModifier orElse onDownloadRequest orElse
    onCheckModifiersToDownload orElse onMissedModifiers orElse super.viewHolderEvents

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      requestDownload(modifierTypeId, modifierId)
  }

  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      deliveryTracker.removeOutdatedToDownload(historyReaderOpt)
      deliveryTracker.downloadRetry().foreach(i => requestDownload(i._2.tp, i._1))

  }

  def requestDownload(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val msg = Message(requestModifierSpec, Right(modifierTypeId -> Seq(modifierId)), None)
    //Full nodes should be here, not a random peer
    networkControllerRef ! SendToNetwork(msg, SendToRandom)
    deliveryTracker.downloadRequested(modifierTypeId, modifierId)
  }
}

object EncryNodeViewSynchronizer {

  case object CheckModifiersToDownload

  case class MissedModifiers(m: Seq[(ModifierTypeId, ModifierId)])
}
