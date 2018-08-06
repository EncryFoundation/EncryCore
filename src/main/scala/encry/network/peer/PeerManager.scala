package encry.network.peer

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.Actor
import encry.EncryApp._
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.{DisconnectedPeer, HandshakedPeer}
import encry.network.NetworkController.ReceivableMessages.ConnectTo
import encry.network.PeerConnectionHandler.ReceivableMessages.{CloseConnection, StartInteraction}
import encry.network.PeerConnectionHandler._
import encry.network.peer.PeerManager.ReceivableMessages._
import encry.network.peer.PeerManager._
import encry.network.{Handshake, SendingStrategy}
import encry.utils.Logging
import scala.language.postfixOps
import scala.util.Random

class PeerManager extends Actor with Logging {

  var connectedPeers: Map[InetSocketAddress, ConnectedPeer] = Map.empty
  var connectingPeers: Set[InetSocketAddress] = Set.empty

  if (PeerDatabase.isEmpty) settings.network.knownPeers.foreach { address =>
    if (!isSelf(address, None)) PeerDatabase.addOrUpdateKnownPeer(address, PeerInfo(timeProvider.time(), None))
  }

  def isSelf(address: InetSocketAddress, declaredAddress: Option[InetSocketAddress]): Boolean =
    settings.network.bindAddress == address ||
      settings.network.declaredAddress.exists(da => declaredAddress.contains(da)) ||
      declaredAddress.contains(settings.network.bindAddress) ||
      settings.network.declaredAddress.contains(address) ||
      (InetAddress.getLocalHost.getAddress sameElements address.getAddress.getAddress) ||
      (InetAddress.getLoopbackAddress.getAddress sameElements address.getAddress.getAddress)

  override def receive: Receive = {
    case GetConnectedPeers => sender() ! (connectedPeers.values.map(_.handshake).toSeq: Seq[Handshake])
    case GetAllPeers => sender() ! PeerDatabase.knownPeers()
    case AddOrUpdatePeer(address, peerNameOpt, connTypeOpt) =>
      if (!isSelf(address, None))
        PeerDatabase.addOrUpdateKnownPeer(address, PeerInfo(timeProvider.time(), peerNameOpt, connTypeOpt))
    case RandomPeers(howMany: Int) => sender() ! Random.shuffle(PeerDatabase.knownPeers().keys.toSeq).take(howMany)
    case FilterPeers(sendingStrategy: SendingStrategy) => sender() ! sendingStrategy.choose(connectedPeers.values.toSeq)
    case DoConnecting(remote, direction) =>
      if (connectingPeers.contains(remote) && direction != Incoming) {
        log.info(s"Trying to connect twice to $remote, going to drop the duplicate connection")
        sender() ! CloseConnection
      }
      else if (direction != Incoming) {
        log.info(s"Connecting to $remote")
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
        val peers: Seq[InetSocketAddress] = PeerDatabase.knownPeers().keys.toSeq
        if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
        else None
      }

      if (connectedPeers.size + connectingPeers.size <= settings.network.maxConnections)
        randomPeer.filter(address => !connectedPeers.exists(_._1 == address) &&
          !connectingPeers.exists(_.getHostName == address.getHostName) && checkPossibilityToAddPeer(address)).foreach { address =>
            sender() ! ConnectTo(address)
        }
  }
}

object PeerManager {

  object ReceivableMessages {

    case object CheckPeers

    case class AddOrUpdatePeer(address: InetSocketAddress, peerName: Option[String], direction: Option[ConnectionType])

    case class RandomPeers(howMany: Int)

    case class FilterPeers(sendingStrategy: SendingStrategy)

    case object GetConnectedPeers

    case object GetAllPeers

    case object GetBlacklistedPeers

    case class DoConnecting(remote: InetSocketAddress, direction: ConnectionType)

    case class Handshaked(peer: ConnectedPeer)

    case class Disconnected(remote: InetSocketAddress)

  }

  def checkPossibilityToAddPeer(address: InetSocketAddress): Boolean =
    (settings.network.connectOnlyWithKnownPeers && settings.network.knownPeers.contains(address)) ||
      !settings.network.connectOnlyWithKnownPeers
}