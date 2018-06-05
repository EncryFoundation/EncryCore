package encry.network.peer

import java.net.InetSocketAddress

import akka.actor.Actor
import scorex.core.network._
import scorex.core.utils.ScorexLogging
import encry.EncryApp._
import PeerManager.ReceivableMessages._
import encry.network.Handshake
import encry.network.NodeViewSynchronizer.ReceivableMessages.{DisconnectedPeer, HandshakedPeer}
import encry.network.NetworkController.ReceivableMessages.ConnectTo
import encry.network.PeerConnectionHandler._
import encry.network.PeerConnectionHandler.ReceivableMessages.{CloseConnection, StartInteraction}

import scala.collection.mutable
import scala.util.Random

class PeerManager extends Actor with ScorexLogging {

  //peers after successful handshake
  val connectedPeers: mutable.Map[InetSocketAddress, ConnectedPeer] = mutable.Map[InetSocketAddress, ConnectedPeer]()

  //peers before handshake
  val connectingPeers: mutable.Set[InetSocketAddress] = mutable.Set[InetSocketAddress]()

  val peerDatabase: PeerDatabase = PeerDatabase(Some(settings.dataDir + "/peers.dat"))

  if (peerDatabase.isEmpty) settings.network.knownPeers.foreach { address =>
    if (!isSelf(address, None)) peerDatabase.addOrUpdateKnownPeer(address, PeerInfo(timeProvider.time(), None))
  }

  def randomPeer(): Option[InetSocketAddress] = {
    val peers: Seq[InetSocketAddress] = peerDatabase.knownPeers().keys.toSeq
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  /**
    * Given a peer's address and declared address, returns `true` iff the peer is the same is this node.
    */
  def isSelf(address: InetSocketAddress, declaredAddress: Option[InetSocketAddress]): Boolean =
    settings.network.bindAddress == address ||
      settings.network.declaredAddress.exists(da => declaredAddress.contains(da)) ||
      declaredAddress.contains(settings.network.bindAddress) ||
      settings.network.declaredAddress.contains(address)

  override def receive: Receive = peerListOperations orElse apiInterface orElse connectOps

  def apiInterface: Receive = {
    case GetConnectedPeers => sender() ! (connectedPeers.values.map(_.handshake).toSeq: Seq[Handshake])
    case GetAllPeers => sender() ! peerDatabase.knownPeers()
    case GetBlacklistedPeers => sender() ! peerDatabase.blacklistedPeers()
  }

  def peerListOperations: Receive = {
    case AddOrUpdatePeer(address, peerNameOpt, connTypeOpt) =>
      if (!isSelf(address, None))
        peerDatabase.addOrUpdateKnownPeer(address, PeerInfo(timeProvider.time(), peerNameOpt, connTypeOpt))
    case KnownPeers => sender() ! peerDatabase.knownPeers().keys.toSeq
    case RandomPeer => sender() ! randomPeer()
    case RandomPeers(howMany: Int) => sender() ! Random.shuffle(peerDatabase.knownPeers().keys.toSeq).take(howMany)
    case FilterPeers(sendingStrategy: SendingStrategy) => sender() ! sendingStrategy.choose(connectedPeers.values.toSeq)
  }

  def connectOps: Receive = {
    case DoConnecting(remote, direction) =>
      if (peerDatabase.isBlacklisted(remote)) log.info(s"Got incoming connection from blacklisted $remote")
      else {
        val isIncoming: Boolean = direction == Incoming
        if (connectingPeers.contains(remote) && !isIncoming) {
          log.info(s"Trying to connect twice to $remote, going to drop the duplicate connection")
          sender() ! CloseConnection
        } else {
          if (!isIncoming) {
            log.info(s"Connecting to $remote")
            connectingPeers += remote
          }
          sender() ! StartInteraction
        }
      }
    case Handshaked(peer) =>
      if (peerDatabase.isBlacklisted(peer.socketAddress))
        log.info(s"Got handshake from blacklisted ${peer.socketAddress}")
       else {
        if (peer.direction == Outgoing && isSelf(peer.socketAddress, peer.handshake.declaredAddress))
          peer.handlerRef ! CloseConnection
        else {
          if (peer.publicPeer) self ! AddOrUpdatePeer(peer.socketAddress, Some(peer.handshake.nodeName), Some(peer.direction))
          else peerDatabase.remove(peer.socketAddress)
          connectedPeers += peer.socketAddress -> peer
          context.system.eventStream.publish(HandshakedPeer(peer))
        }
      }
    case Disconnected(remote) =>
      connectedPeers -= remote
      connectingPeers -= remote
      context.system.eventStream.publish(DisconnectedPeer(remote))
    case CheckPeers =>
      if (connectedPeers.size + connectingPeers.size < settings.network.maxConnections) {
        randomPeer().foreach { address =>
          if (!connectedPeers.exists(_._1 == address) &&
            !connectingPeers.exists(_.getHostName == address.getHostName)) {
            sender() ! ConnectTo(address)
          }
        }
      }
    case AddToBlacklist(peer) =>
      log.info(s"Blacklist peer $peer")
      peerDatabase.blacklistPeer(peer, timeProvider.time())
  }
}

object PeerManager {

  object ReceivableMessages {

    case object CheckPeers

    case class AddToBlacklist(remote: InetSocketAddress)

    case class AddOrUpdatePeer(address: InetSocketAddress, peerName: Option[String], direction: Option[ConnectionType])

    case object KnownPeers

    case object RandomPeer

    case class RandomPeers(hawMany: Int)

    case class FilterPeers(sendingStrategy: SendingStrategy)

    case object GetConnectedPeers

    case object GetAllPeers

    case object GetBlacklistedPeers

    case class DoConnecting(remote: InetSocketAddress, direction: ConnectionType)

    case class Handshaked(peer: ConnectedPeer)

    case class Disconnected(remote: InetSocketAddress)

  }

}