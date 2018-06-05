package encry.local

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import encry.Version
import encry.consensus.NBits
import encry.local.EncryLocalInterface.{GetNodeInfo, NodeInfo}
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.network.Handshake
import encry.settings.{Algos, Constants, EncryAppSettings}
import encry.view.history.EncryHistory
import encry.view.state.StateMode
import io.circe.Json
import io.circe.syntax._
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedMempool, ChangedState, SemanticallySuccessfulModifier}
import encry.network.peer.PeerManager.ReceivableMessages.GetConnectedPeers
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * Class that subscribes to NodeViewHolderEvents and collects them to provide fast response to API requests.
  */
class EncryLocalInterface(viewHolderRef: ActorRef,
                          peerManager: ActorRef,
                          settings: EncryAppSettings,
                          timeProvider: NetworkTimeProvider)
  extends Actor with ScorexLogging{

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ChangedHistory[_]])
    context.system.eventStream.subscribe(self, classOf[ChangedMempool[_]])
    context.system.eventStream.subscribe(self, classOf[ChangedState[_]])
    context.system.scheduler.schedule(10.second, 10.second)(peerManager ! GetConnectedPeers)
  }

  private val votes = Algos.encode(Algos.hash(settings.network.nodeName).take(5))

  var nodeInfo = NodeInfo(settings.network.nodeName, Version.VersionString, 0, 0, "null",
    settings.node.stateMode, "null", isMining = settings.node.mining, votes, None, None,
    timeProvider.time())

  override def receive: Receive = onConnectedPeers orElse getNodeInfo orElse onMempoolChanged orElse
    onHistoryChanged orElse onSemanticallySuccessfulModification

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

  def onSemanticallySuccessfulModification: Receive = {
    case SemanticallySuccessfulModifier(fb: EncryBlock) =>
      nodeInfo = nodeInfo.copy(stateRoot = Algos.encode(fb.header.stateRoot),
        stateVersion = fb.encodedId)
  }
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
      "difficulty" -> bestFullBlockOpt.map(_.header.nBits.untag(NBits)).getOrElse(Constants.Chain.InitialNBits).asJson,
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
