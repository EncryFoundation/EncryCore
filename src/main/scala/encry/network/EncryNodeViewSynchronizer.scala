package encry.network

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proposition.EncryProposition
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.EncryMempool
import encry.EncryApp._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import NodeViewSynchronizer.ReceivableMessages._
import encry.network.message.BasicMsgDataTypes.ModifiersData
import encry.network.message.{Message, MessageSpec, ModifiersSpec}
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.{GetNodeViewChanges, ModifiersFromRemote}
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.crypto.encode.Base58

class EncryNodeViewSynchronizer(syncInfoSpec: EncrySyncInfoMessageSpec.type) extends
  NodeViewSynchronizer[EncryProposition, EncryBaseTransaction, EncrySyncInfo, EncrySyncInfoMessageSpec.type,
    EncryPersistentModifier, EncryHistory, EncryMempool](syncInfoSpec) {

  case object CheckModifiersToDownload

  override val deliveryTracker: EncryDeliveryTracker = EncryDeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, self, timeProvider)

  override def preStart(): Unit = {
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, ModifiersSpec, syncInfoSpec)
    networkController ! RegisterMessagesHandler(messageSpecs, self)
    context.system.eventStream.subscribe(self, classOf[DisconnectedPeer])
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolder ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)
    statusTracker.scheduleSendSyncInfo()
    context.system.scheduler.schedule(settings.network.syncInterval, settings.network.syncInterval)(self ! CheckModifiersToDownload)
  }

  override def viewHolderEvents: Receive =
    onSyntacticallySuccessfulModifier orElse
      onDownloadRequest orElse
      onCheckModifiersToDownload orElse
      super.viewHolderEvents

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      requestDownload(modifierTypeId, Seq(modifierId))
  }

  def onSyntacticallySuccessfulModifier: Receive = {
    case SyntacticallySuccessfulModifier(mod) if (mod.isInstanceOf[EncryBlockHeader] || mod.isInstanceOf[EncryBlockPayload]) &&
      historyReaderOpt.exists(_.isHeadersChainSynced) => broadcastModifierInv(mod)
  }

  def onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      deliveryTracker.removeOutdatedExpectingFromRandom()
      historyReaderOpt.foreach { h =>
        val currentQueue: Iterable[ModifierId] = deliveryTracker.expectingFromRandomQueue
        val newIds: Seq[(ModifierTypeId, ModifierId)] = h.modifiersToDownload(settings.network.networkChunkSize - currentQueue.size, currentQueue)
        val oldIds: Seq[(ModifierTypeId, ModifierId)] = deliveryTracker.idsExpectingFromRandomToRetry()
        (newIds ++ oldIds).groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
  }

  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    modifierIds.foreach(id => deliveryTracker.expectFromRandom(modifierTypeId, id))
    val msg: Message[(ModifierTypeId, Seq[ModifierId])] = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
    networkController ! SendToNetwork(msg, SendToRandom)
  }

  def modifiersFromRemote: Receive = {
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      val typeId: ModifierTypeId = data._1
      val modifiers: Map[ModifierId, Array[Byte]] = data._2
      log.info(s"Got modifiers of type $typeId from remote connected peer: $remote")
      log.trace(s"Received modifier ids ${data._2.keySet.map(Base58.encode).mkString(",")}")
      for ((id, _) <- modifiers) deliveryTracker.receive(typeId, id, remote)
      val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
        modifiers partition { case (id, _) => deliveryTracker.isSpam(id) }
      if (spam.nonEmpty) {
        log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(Base58.encode)}")
        deliveryTracker.deleteSpam(spam.keys.toSeq)
      }
      if (fm.nonEmpty) nodeViewHolder ! ModifiersFromRemote(remote, typeId, fm.values.toSeq)
      //If queue is empty - check, whether there are more modifiers to download
      historyReaderOpt foreach { h =>
        if (!h.isHeadersChainSynced && !deliveryTracker.isExpecting) sendSync(h.syncInfo)
        else if (h.isHeadersChainSynced && !deliveryTracker.isExpectingFromRandom) self ! CheckModifiersToDownload
      }
  }
}