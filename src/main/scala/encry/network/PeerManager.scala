package encry.network

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.Actor
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp._
import encry.cli.commands.AddPeer.PeerFromCli
import encry.network.NodeViewSynchronizer.ReceivableMessages.{DisconnectedPeer, HandshakedPeer}
import encry.network.NetworkController.ReceivableMessages.ConnectTo
import encry.network.PeerConnectionHandler.ReceivableMessages.{CloseConnection, StartInteraction}
import encry.network.PeerConnectionHandler._
import encry.network.PeerManager.ReceivableMessages._
import encry.network.PeerManager._
import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.Random

class PeerManager extends Actor with StrictLogging {

  var connectedPeers: Map[InetSocketAddress, ConnectedPeer] = Map.empty
  var connectingPeers: Set[InetSocketAddress] = Set.empty
  var nodes: Map[InetSocketAddress, PeerInfo] = Map.empty
  val knownPeersObj: KnownPeers.type = KnownPeers

  addKnownPeersToPeersDatabase()

  override def receive: Receive = {
    case PeerFromCli(address) =>
      knownPeersObj.knownPeers = knownPeersObj.knownPeers :+ address
      if (checkDuplicateIP(address))
        context.system.actorSelection("/user/networkController") ! ConnectTo(address)
    case GetConnectedPeers => sender() ! connectedPeers.values.toSeq
    case GetAllPeers => sender() ! knownPeers()
    case AddOrUpdatePeer(address, peerNameOpt, connTypeOpt) =>
      if (!isSelf(address, None) &&
        checkPossibilityToAddPeer(address) &&
        checkDuplicateIP(address)) timeProvider
        .time()
        .map { time => addOrUpdateKnownPeer(address, PeerInfo(time, peerNameOpt, connTypeOpt))
        }
    case RandomPeers(howMany: Int) => sender() ! Random.shuffle(knownPeers().keys.toSeq).take(howMany)
    case FilterPeers(sendingStrategy: SendingStrategy) => sender() ! sendingStrategy.choose(connectedPeers.values.toSeq)
    case DoConnecting(remote, direction) =>
      if (connectingPeers.contains(remote) && direction != Incoming) {
        logger.info(s"Trying to connect twice to $remote, going to drop the duplicate connection")
        sender() ! CloseConnection
      }
      else if (direction != Incoming) {
        logger.info(s"Connecting to $remote")
        connectingPeers += remote
      }
      sender() ! StartInteraction
    case Handshaked(peer) =>
      if (peer.direction == Outgoing && isSelf(peer.socketAddress, peer.handshake.declaredAddress))
        peer.handlerRef ! CloseConnection
      else if (checkPossibilityToAddPeer(peer.socketAddress) && !connectedPeers.contains(peer.socketAddress)) {
        self ! AddOrUpdatePeer(peer.socketAddress, Some(peer.handshake.nodeName), Some(peer.direction))
        connectedPeers += (peer.socketAddress -> peer)
        nodeViewSynchronizer ! HandshakedPeer(peer)
      }
    case Disconnected(remote) =>
      connectedPeers -= remote
      connectingPeers -= remote
      nodeViewSynchronizer ! DisconnectedPeer(remote)
    case CheckPeers =>
      def randomPeer: Option[InetSocketAddress] = {
        val peers: Seq[InetSocketAddress] = knownPeers().keys.toSeq
        if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
        else None
      }

      if (connectedPeers.size + connectingPeers.size <= settings.network.maxConnections)
        randomPeer.filter(address => !connectedPeers.exists(_._1 == address) &&
          !connectingPeers.exists(_.getHostName == address.getHostName) &&
          checkPossibilityToAddPeer(address) &&
          checkDuplicateIP(address)
        ).foreach { address => sender() ! ConnectTo(address) }
  }

  def isSelf(address: InetSocketAddress, declaredAddress: Option[InetSocketAddress]): Boolean =
    settings.network.bindAddress == address ||
      settings.network.declaredAddress.exists(da => declaredAddress.contains(da)) ||
      declaredAddress.contains(settings.network.bindAddress) ||
      settings.network.declaredAddress.contains(address) ||
      (InetAddress.getLocalHost.getAddress sameElements address.getAddress.getAddress) ||
      (InetAddress.getLoopbackAddress.getAddress sameElements address.getAddress.getAddress)

  /// TODO: REMOVE THIS
  def addKnownPeersToPeersDatabase(): Future[Unit] = if (nodes.isEmpty) timeProvider.time().map { time =>
    settings.network.knownPeers.filterNot(isSelf(_, None)).foreach(addOrUpdateKnownPeer(_, PeerInfo(time, None)))
    Unit
  } else Future.successful(Unit)

  def checkDuplicateIP(address: InetSocketAddress): Boolean =
    !connectedPeers.map(_._1.getAddress).toSet.contains(address.getAddress)

  def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo: PeerInfo = nodes.get(address).fold(peerInfo) { dbPeerInfo =>
      val nodeNameOpt: Option[String] = peerInfo.nodeName orElse dbPeerInfo.nodeName
      val connTypeOpt: Option[ConnectionType] = peerInfo.connectionType orElse dbPeerInfo.connectionType
      PeerInfo(peerInfo.lastSeen, nodeNameOpt, connTypeOpt)
    }
    nodes += (address -> updatedPeerInfo)
  }

  def knownPeers(): Map[InetSocketAddress, PeerInfo] = nodes.keys.flatMap(k => nodes.get(k).map(v => k -> v)).toMap
}

object PeerManager {

  object ReceivableMessages {

    case object CheckPeers

    case class AddOrUpdatePeer(address: InetSocketAddress, peerName: Option[String], direction: Option[ConnectionType])

    case class RandomPeers(howMany: Int)

    case object GetRecoveryStatus

    case class FilterPeers(sendingStrategy: SendingStrategy)

    case object GetConnectedPeers

    case object GetAllPeers

    case class DoConnecting(remote: InetSocketAddress, direction: ConnectionType)

    case class Handshaked(peer: ConnectedPeer)

    case class Disconnected(remote: InetSocketAddress)

  }

  def checkPossibilityToAddPeer(address: InetSocketAddress): Boolean =
    (settings.network.connectOnlyWithKnownPeers.getOrElse(false) &&
      KnownPeers.knownPeers.map(_.getAddress).contains(address.getAddress)) ||
      !settings.network.connectOnlyWithKnownPeers.getOrElse(false)
}

/// TODO: REMOVE THIS
case class PeerInfo(lastSeen: Long, nodeName: Option[String] = None, connectionType: Option[ConnectionType] = None)

case object KnownPeers {
  var knownPeers: Seq[InetSocketAddress] = settings.network.knownPeers
}