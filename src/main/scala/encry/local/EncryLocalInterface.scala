package encry.local

import akka.actor.{ActorRef, ActorSystem, Props}
import encry.Version
import encry.local.EncryLocalInterface.{GetNodeInfo, NodeInfo}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proposition.EncryProposition
import encry.settings.{Algos, EncryAppSettings}
import encry.view.history.EncryHistory
import encry.view.state.StateMode
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewHolder.ReceivableMessages.Subscribe
import scorex.core.network.Handshake
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedMempool}
import scorex.core.network.peer.PeerManager
import scorex.core.network.peer.PeerManager.ReceivableMessages.GetConnectedPeers
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{LocalInterface, ModifierId, NodeViewHolder}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * Class that subscribes to NodeViewHolderEvents and collects them to provide fast response to API requests.
  */
class EncryLocalInterface(override val viewHolderRef: ActorRef,
                          peerManager: ActorRef,
                          settings: EncryAppSettings,
                          timeProvider: NetworkTimeProvider)
  extends LocalInterface[EncryProposition, EncryBaseTransaction, EncryPersistentModifier] {

  override def preStart(): Unit = {
    val events = Seq(
      NodeViewHolder.EventType.StateChanged,
      NodeViewHolder.EventType.HistoryChanged,
      NodeViewHolder.EventType.MempoolChanged
    )
    viewHolderRef ! Subscribe(events)
    context.system.scheduler.schedule(10.second, 10.second)(peerManager ! GetConnectedPeers)
  }

  private val votes = Algos.encode(Algos.hash(settings.scorexSettings.network.nodeName).take(5))

  var nodeInfo = NodeInfo(settings.scorexSettings.network.nodeName, Version.VersionString, 0, 0, "null",
    settings.nodeSettings.stateMode, "null", isMining = settings.nodeSettings.mining, votes, None, None,
    timeProvider.time())

  override def receive: Receive = onConnectedPeers orElse getNodeInfo orElse onMempoolChanged orElse
    onHistoryChanged orElse super.receive

  private def getNodeInfo: Receive = {
    case GetNodeInfo => sender ! nodeInfo
  }

  private def onMempoolChanged: Receive = {
    case ChangedMempool(p) =>
      nodeInfo = nodeInfo.copy(unconfirmedCount = p.size)
  }

  private def onHistoryChanged: Receive = {
    case ChangedHistory(h) if h.isInstanceOf[EncryHistory] =>
      nodeInfo = nodeInfo.copy(bestFullBlockOpt = h.asInstanceOf[EncryHistory].bestBlockOpt,
        bestHeaderOpt = h.asInstanceOf[EncryHistory].bestHeaderOpt)
  }

  private def onConnectedPeers: Receive = {
    case peers: Seq[Handshake@unchecked] if peers.headOption.forall(_.isInstanceOf[Handshake]) =>
      nodeInfo = nodeInfo.copy(peersCount = peers.length)
  }

  override protected def onStartingPersistentModifierApplication(pmod: EncryPersistentModifier): Unit = {}

  override protected def onFailedTransaction(tx: EncryBaseTransaction): Unit = {}

  override protected def onSuccessfulTransaction(tx: EncryBaseTransaction): Unit = {}

  override protected def onNoBetterNeighbour(): Unit = {}

  override protected def onBetterNeighbourAppeared(): Unit = {}

  override protected def onSyntacticallySuccessfulModification(mod: EncryPersistentModifier): Unit = {}

  override protected def onSyntacticallyFailedModification(mod: EncryPersistentModifier): Unit = {}

  override protected def onSemanticallySuccessfulModification(mod: EncryPersistentModifier): Unit = {}

  override protected def onSemanticallyFailedModification(mod: EncryPersistentModifier): Unit = {}

  override protected def onNewSurface(newSurface: Seq[ModifierId]): Unit = {}

  override protected def onRollbackFailed(): Unit = {}
}

object EncryLocalInterface {

  case object GetNodeInfo

  case class NodeInfo(nodeName: String,
                      appVersion: String,
                      unconfirmedCount: Int,
                      peersCount: Int,
                      stateRoot: String,
                      stateType: StateMode,
                      stateVersion: String,
                      isMining: Boolean,
                      votes: String,
                      bestHeaderOpt: Option[EncryBlockHeader],
                      bestFullBlockOpt: Option[EncryBlock],
                      launchTime: Long) {

    lazy val json: Json = Map(
      "name" -> nodeName.asJson,
      "appVersion" -> Version.VersionString.asJson,
      "headersHeight" -> bestHeaderOpt.map(_.height.toString).getOrElse("null").asJson,
      "fullHeight" -> bestFullBlockOpt.map(_.header.height.toString).getOrElse("null").asJson,
      "bestHeaderId" -> bestHeaderOpt.map(_.encodedId).getOrElse("null").asJson,
      "bestFullHeaderId" -> bestFullBlockOpt.map(_.header.encodedId).getOrElse("null").asJson,
      "previousFullHeaderId" -> bestFullBlockOpt.map(_.header.parentId).map(Algos.encode).getOrElse("null").asJson,
      "difficulty" -> bestFullBlockOpt.map(_.header.difficulty.toString(10)).getOrElse("null").asJson,
      "unconfirmedCount" -> unconfirmedCount.asJson,
      "stateRoot" -> stateRoot.asJson,
      "stateType" -> stateType.verboseName.asJson,
      "stateVersion" -> stateVersion.asJson,
      "isMining" -> isMining.asJson,
      "votes" -> votes.asJson,
      "peersCount" -> peersCount.asJson,
      "launchTime" -> launchTime.asJson
    ).asJson
  }
}

object EncryLocalInterfaceRef {

  def props(viewHolderRef: ActorRef,
            peerManager: ActorRef,
            settings: EncryAppSettings,
            timeProvider: NetworkTimeProvider): Props =
    Props(new EncryLocalInterface(viewHolderRef, peerManager, settings, timeProvider))

  def apply(viewHolderRef: ActorRef, peerManager: ActorRef, settings: EncryAppSettings, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(viewHolderRef, peerManager, settings, timeProvider))

  def apply(name: String,
            viewHolderRef: ActorRef,
            peerManager: ActorRef,
            settings: EncryAppSettings,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(viewHolderRef, peerManager, settings, timeProvider), name)
}
