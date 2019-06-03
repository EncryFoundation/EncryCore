package encry.network

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.network.BlackList.BanReason
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.PeerConnectionHandler._
import encry.network.PeersKeeper._
import encry.settings.EncryAppSettings
import encry.EncryApp.{networkController, nodeViewSynchronizer}
import encry.consensus.History.HistoryComparisonResult
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PrioritiesCalculator.AccumulatedPeersStatistic
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus
import org.encryfoundation.common.network.BasicMessagesRepo._

import scala.concurrent.duration._
import scala.util.Random

class PeersKeeper(settings: EncryAppSettings) extends Actor with StrictLogging {

  import context.dispatcher

  var availablePeers: Map[InetAddress, InetSocketAddress] = settings.network.knownPeers.map(p => p.getAddress -> p).toMap

  val connectWithOnlyKnownPeers: Boolean = settings.network.connectOnlyWithKnownPeers.getOrElse(true)

  val connectedPeers: ConnectedPeersList = new ConnectedPeersList(settings)

  val blackList: BlackList = new BlackList(settings)

  var outgoingConnections: Set[InetAddress] = Set.empty

  override def preStart(): Unit = {
    networkController ! RegisterMessagesHandler(Seq(
      PeersNetworkMessage.NetworkMessageTypeID -> "PeersNetworkMessage",
      GetPeersNetworkMessage.NetworkMessageTypeID -> "GetPeersNetworkMessage"
    ), self)
    if (!connectWithOnlyKnownPeers)
      context.system.scheduler.schedule(2.seconds, settings.network.syncInterval)(
        self ! SendToNetwork(GetPeersNetworkMessage, SendToRandom)
      )
    context.system.scheduler.schedule(10.seconds, 10.seconds)(blackList.cleanupBlackList())
    context.system.scheduler.schedule(settings.network.syncInterval, settings.network.syncInterval)(
      self ! SendLocalSyncInfo
    )
    context.system.scheduler.schedule(10.seconds, 5.seconds)(
      nodeViewSynchronizer ! UpdatedPeersCollection(connectedPeers.getPeersForDeliveryManager)
    )
  }

  override def receive: Receive = setupConnectionsLogic
    .orElse(networkMessagesProcessingLogic)
    .orElse(banPeersLogic)
    .orElse {
      case OtherNodeSyncingStatus(remote, comparison, _) => connectedPeers.updatePeerComparisonStatus(remote, comparison)
      case AccumulatedPeersStatistic(statistic) => connectedPeers.updatePeersPriorityStatus(statistic)
      case SendToNetwork(message, strategy) =>
        strategy.choose(connectedPeers.getAllConnectedPeers).foreach { peer =>
          logger.info(s"Sending message: ${message.messageName} to: ${peer.socketAddress}.")
          peer.handlerRef ! message
        }
      case SendLocalSyncInfo =>
        logger.info(s"Time to send sync info!")
        nodeViewSynchronizer ! PeersForSyncInfo(connectedPeers.getAllConnectedPeers)
      case msg => logger.info(s"Peers keeper got unhandled message: $msg.")
    }

  def setupConnectionsLogic: Receive = {
    case RequestPeerForConnection if connectedPeers.size < settings.network.maxConnections =>
      logger.info(s"Got request for new connection. Current number of connections is: ${connectedPeers.size}, " +
        s"so peer keeper allows to add one more connect.")
      Random.shuffle(availablePeers).headOption.foreach { peer =>
        logger.info(s"Selected peer: ${peer._1}. Sending 'PeerForConnection' message to network controller. " +
          s"Adding new outgoing connection to outgoingConnections collection. Current collection is: " +
          s"${outgoingConnections.mkString(",")}.")
        sender() ! PeerForConnection(peer._2)
        outgoingConnections += peer._1
        availablePeers -= peer._1
      }

    case RequestPeerForConnection =>
      logger.info(s"Got request for a new connection but current number of connection is max: ${connectedPeers.size}.")

    case RequestForStableConnection(remote, remoteConnection) if connectedPeers.size < settings.network.maxConnections =>
      logger.info(s"Peers keeper got request for a stable connection with remote: $remote.")
      val notConnectedYet: Boolean = !connectedPeers.contains(remote.getAddress)
      val notBannedPeer: Boolean = !blackList.contains(remote.getAddress)
      if (notConnectedYet && notBannedPeer) {
        //todo check for duplicates
        logger.info(s"Peer: $remote is available to setup stable connect with it. Sending approvement for connection.")
        val connectionType: ConnectionType = if (outgoingConnections.contains(remote.getAddress)) Outgoing else Incoming
        if (connectionType == Incoming && connectWithOnlyKnownPeers)
          logger.info(s"Got incomming connection but we can connect only with known peers.")
        else {
          outgoingConnections -= remote.getAddress
          sender() ! CreateStableConnection(remote, remoteConnection, connectionType)
        }
      } else logger.info(s"Connection for requested peer: $remote is unavailable cause of:" +
        s" Is banned: $notBannedPeer, Is connected: $notConnectedYet.")

    case RequestForStableConnection(_, _) =>
      logger.info(s"Peers keeper got request for a stable connection but current number of max connection is bigger than possible.")

    case StableConnectionSetup(connectedPeer) =>
      logger.info(s"Peers keeper got approvement about stable connection. Initializing new peer.")
      connectedPeers.initializePeer(connectedPeer)

    case ConnectionStopped(peer) =>
      logger.info(s"Connection stopped for: $peer.")
      connectedPeers.removePeer(peer.getAddress)
      if (!blackList.contains(peer.getAddress)) availablePeers += peer.getAddress -> peer

    case OutgoingConnectionFailed(peer) =>
      logger.info(s"Connection failed for: $peer.")
      outgoingConnections -= peer.getAddress
      availablePeers += peer.getAddress -> peer
  }

  def networkMessagesProcessingLogic: Receive = {
    case DataFromPeer(message, remote) => message match {
      case PeersNetworkMessage(peers) if !connectWithOnlyKnownPeers =>
        peers.filterNot(p => blackList.contains(p.getAddress) && connectedPeers.contains(p.getAddress)).foreach { p =>
          logger.info(s"Found new peer: $p. Adding it to the available peers collection.")
          availablePeers = availablePeers.updated(p.getAddress, p)
        }
      case PeersNetworkMessage(_) => //todo do ban
      case GetPeersNetworkMessage =>
        val correctPeers: Seq[InetSocketAddress] = connectedPeers.getAll.filter(address =>
          if (remote.socketAddress.getAddress.isSiteLocalAddress) true
          else address.getAddress.isSiteLocalAddress && address != remote.socketAddress
        )
        logger.info(s"Got request for local known peers. Sending to: $remote peers: ${correctPeers.mkString(",")}.")
        remote.handlerRef ! PeersNetworkMessage(correctPeers)
    }
  }

  def banPeersLogic: Receive = {
    case BanPeer(peer, reason) =>
      logger.info(s"Banning peer: ${peer.socketAddress} for $reason.")
      blackList.banPeer(reason, peer.socketAddress.getAddress)
  }

}

object PeersKeeper {

  final case class RequestForStableConnection(peer: InetSocketAddress, remoteConnection: ActorRef)

  final case class CreateStableConnection(peer: InetSocketAddress, remoteConnection: ActorRef, ct: ConnectionType)

  final case class OutgoingConnectionFailed(peer: InetSocketAddress) extends AnyVal

  final case class StableConnectionSetup(peer: ConnectedPeer) extends AnyVal

  final case class ConnectionStopped(peer: InetSocketAddress) extends AnyVal


  final case object RequestPeerForConnection

  final case class PeerForConnection(peer: InetSocketAddress) extends AnyVal


  final case class SendToNetwork(message: NetworkMessage, sendingStrategy: SendingStrategy)

  final case class PeersForSyncInfo(peers: Seq[ConnectedPeer]) extends AnyVal

  final case class UpdatedPeersCollection(peers: Map[InetAddress, (ConnectedPeer, HistoryComparisonResult, PeersPriorityStatus)]) extends AnyVal

  final case class BanPeer(peer: ConnectedPeer, reason: BanReason)


  final case object RequestAllAvailablePeers

  final case class AllAvailablePeers(peers: Seq[InetSocketAddress]) extends AnyVal

  final case class RequestRandomAvailablePeers(qty: Int) extends AnyVal

  final case class RandomAvailablePeers(peers: Seq[InetSocketAddress]) extends AnyVal


  def props(settings: EncryAppSettings): Props = Props(new PeersKeeper(settings))
}