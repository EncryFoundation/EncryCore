package encry.network

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.{ConnectedPeersConnectionHelper, UpdatingPeersInfo}
import encry.consensus.HistoryConsensus.HistoryComparisonResult
import encry.network.BlackList.BanReason.SentPeersMessageWithoutRequest
import encry.network.BlackList.{BanReason, BanTime, BanType}
import encry.network.ConnectedPeersCollection.{LastUptime, PeerInfo}
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler._
import encry.network.PeerConnectionHandler.ReceivableMessages.CloseConnection
import encry.network.PeersKeeper._
import encry.network.PrioritiesCalculator.AccumulatedPeersStatistic
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus.{HighPriority, InitialPriority}
import encry.settings.EncryAppSettings
import org.encryfoundation.common.network.BasicMessagesRepo._

import scala.concurrent.duration._
import scala.util.{Random, Try}

class PeersKeeper(settings: EncryAppSettings,
                  nodeViewSync: ActorRef,
                  dataHolder: ActorRef) extends Actor with StrictLogging {

  import context.dispatcher

  val connectWithOnlyKnownPeers: Boolean = settings.network.connectOnlyWithKnownPeers.getOrElse(true)

  var connectedPeers: ConnectedPeersCollection = ConnectedPeersCollection()

  var blackList: BlackList = BlackList(settings)

  var knownPeers: Set[InetAddress] = settings.network.knownPeers
    .collect { case peer: InetSocketAddress if !isSelf(peer) => peer.getAddress }.toSet

  //todo behaviour is incorrect while outgoing connection with connectWithOnlyKnownPeers param
  var peersForConnection: Map[InetSocketAddress, Int] = settings.network.knownPeers
    .collect { case peer: InetSocketAddress if !isSelf(peer) => peer -> 0 }.toMap

  var awaitingHandshakeConnections: Set[InetSocketAddress] = Set.empty

  var outgoingConnections: Set[InetSocketAddress] = Set.empty

  override def preStart(): Unit = {
    nodeViewSync ! RegisterMessagesHandler(Seq(
      PeersNetworkMessage.NetworkMessageTypeID -> "PeersNetworkMessage",
      GetPeersNetworkMessage.NetworkMessageTypeID -> "GetPeersNetworkMessage"
    ), self)
    if (!connectWithOnlyKnownPeers) context.system.scheduler.schedule(2.seconds, settings.network.syncInterval)(
      self ! SendToNetwork(GetPeersNetworkMessage, SendToRandom)
    )
    context.system.scheduler.schedule(600.millis, settings.blackList.cleanupTime){blackList = blackList.cleanupBlackList}
    context.system.scheduler.schedule(10.seconds, 5.seconds) (dataHolder ! ConnectedPeersConnectionHelper(connectedPeers))
    context.system.scheduler.schedule(10.seconds, 5.seconds)(
      nodeViewSync ! UpdatedPeersCollection(connectedPeers.collect(getAllPeers, getPeersForDM).toMap)
    )
    context.system.eventStream.subscribe(self, classOf[PeerCommandHelper])
    context.system.scheduler.schedule(5.seconds, 5.seconds){
      dataHolder ! UpdatingPeersInfo(
        peersForConnection.keys.toSeq,
        connectedPeers.collect(getAllPeers, getConnectedPeers),
        blackList.getAll
      )
  }
  }

  override def receive: Receive = workingBehaviour(isBlockChainSynced = false)

  def workingBehaviour(isBlockChainSynced: Boolean): Receive = setupConnectionsLogic
    .orElse(networkMessagesProcessingLogic)
    .orElse(banPeersLogic)
    .orElse(additionalMessages(isBlockChainSynced))

  def setupConnectionsLogic: Receive = {
    case RequestPeerForConnection if connectedPeers.size < settings.network.maxConnections =>
      def mapReason(address: InetAddress, r: BanReason, t: BanTime, bt: BanType): (InetAddress, BanReason) = address -> r
      logger.info(s"Got request for new connection. Current number of connections is: ${connectedPeers.size}, " +
        s"so peer keeper allows to add one more connection. Current available peers are: " +
        s"${peersForConnection.mkString(",")}. Current black list is: ${
          blackList.collect((_, _, _, _) => true, mapReason).mkString(",")
        }. Current known peers: ${knownPeers.mkString(",")}.")
      logger.info(s"awaitingHandshakeConnections ${awaitingHandshakeConnections.mkString(",")}")
      logger.info(s"connectedPeers.getAll ${connectedPeers.getAll.mkString(",")}")
      val peers = peersForConnection
        .filterNot(p => awaitingHandshakeConnections.contains(p._1) || connectedPeers.contains(p._1))
      logger.info(s"peers size: ${peers.size}")
      Random.shuffle(peers.toSeq)
        .headOption
        .foreach { case (peer, _) =>
          outgoingConnections += peer
          logger.info(s"Selected peer: $peer. Sending 'PeerForConnection' message to network controller. " +
            s"Adding new outgoing connection to outgoingConnections collection. Current collection is: " +
            s"${outgoingConnections.mkString(",")}.")
          sender() ! PeerForConnection(peer)
          awaitingHandshakeConnections += peer
          logger.info(s"Adding new peer: $peer to awaitingHandshakeConnections." +
            s" Current is: ${awaitingHandshakeConnections.mkString(",")}")
        }

    case RequestPeerForConnection =>
      logger.info(s"Got request for a new connection but current number of connection is max: ${connectedPeers.size}.")

    case NewConnection(remote, remoteConnection) if connectedPeers.size < settings.network.maxConnections && !isSelf(remote) =>
      logger.info(s"Peers keeper got request for verifying the connection with remote: $remote. " +
        s"Remote InetSocketAddress is: $remote. Remote InetAddress is ${remote.getAddress}. " +
        s"Current known peers: ${knownPeers.mkString(",")}")
      val notConnectedYet: Boolean = !connectedPeers.contains(remote)
      val notBannedPeer: Boolean = !blackList.contains(remote.getAddress)
      if (notConnectedYet && notBannedPeer) {
        logger.info(s"Peer: $remote is available to setup connect with.")
        if (outgoingConnections.contains(remote)) {
          logger.info(s"Got outgoing connection.")
          outgoingConnections -= remote
          sender() ! ConnectionVerified(remote, remoteConnection, Outgoing)
        }
        else if (connectWithOnlyKnownPeers && knownPeers.contains(remote.getAddress)) {
          logger.info(s"connectWithOnlyKnownPeers - true, but connected peer is contained in known peers collection.")
          sender() ! ConnectionVerified(remote, remoteConnection, Incoming)
        }
        else if (connectWithOnlyKnownPeers)
          logger.info(s"Got incoming connection but we can connect only with known peers.")
        else {
          logger.info(s"Got new incoming connection. Sending to network controller approvement for connect.")
          sender() ! ConnectionVerified(remote, remoteConnection, Incoming)
        }
      } else logger.info(s"Connection for requested peer: $remote is unavailable cause of:" +
        s" Didn't banned: $notBannedPeer, Didn't connected: $notConnectedYet.")

    case NewConnection(remote, remoteConnection) =>
      logger.info(s"Peers keeper got request for verifying the connection but current number of max connection is " +
        s"bigger than possible or isSelf: ${isSelf(remote)}.")

    case HandshakedDone(connectedPeer) =>
      logger.info(s"Peers keeper got approvement about finishing a handshake." +
        s" Initializing new peer: ${connectedPeer.socketAddress}")
      connectedPeers = connectedPeers.initializePeer(connectedPeer)
      logger.info(s"Remove  ${connectedPeer.socketAddress} from awaitingHandshakeConnections collection. Current is: " +
        s"${awaitingHandshakeConnections.mkString(",")}.")
      awaitingHandshakeConnections -= connectedPeer.socketAddress
      peersForConnection = peersForConnection.updated(connectedPeer.socketAddress, 0)
      logger.info(s"Adding new peer: ${connectedPeer.socketAddress} to available collection." +
        s" Current collection is: ${peersForConnection.keys.mkString(",")}.")

    case ConnectionStopped(peer) =>
      logger.info(s"Connection stopped for: $peer.")
      awaitingHandshakeConnections -= peer
      connectedPeers = connectedPeers.removePeer(peer)
      if (blackList.contains(peer.getAddress)) {
        peersForConnection -= peer
        logger.info(s"Peer: $peer removed from availablePeers cause of it has been banned. " +
          s"Current is: ${peersForConnection.mkString(",")}.")
      }

    case OutgoingConnectionFailed(peer) =>
      logger.info(s"Connection failed for: $peer.")
      outgoingConnections -= peer
      awaitingHandshakeConnections -= peer
      val connectionAttempts: Int = peersForConnection.getOrElse(peer, 0) + 1
      if (connectionAttempts >= settings.network.maxNumberOfReConnections) {
        logger.info(s"Removing peer: $peer from available peers for ExpiredNumberOfConnections.")
        //todo think about penalty for the less time than general ban
        //blackList.banPeer(ExpiredNumberOfConnections, peer.getAddress)
        peersForConnection -= peer
      } else peersForConnection = peersForConnection.updated(peer, connectionAttempts)
  }

  def networkMessagesProcessingLogic: Receive = {
    case DataFromPeer(message, remote) => message match {
      case PeersNetworkMessage(peers) if !connectWithOnlyKnownPeers =>
        logger.info(s"Got peers message from $remote with peers ${peers.mkString(",")}")
        peers
          .filterNot { p =>
            blackList.contains(p.getAddress) || connectedPeers.contains(p) || isSelf(p) || peersForConnection.contains(p)
          }.foreach { p =>
            logger.info(s"Found new peer: $p. Adding it to the available peers collection.")
            peersForConnection = peersForConnection.updated(p, 0)
          }
        logger.info(s"New available peers collection after processing peers from $remote is: ${peersForConnection.keys.mkString(",")}.")

      case PeersNetworkMessage(_) =>
        logger.info(s"Got PeersNetworkMessage from $remote, but connectWithOnlyKnownPeers: $connectWithOnlyKnownPeers, " +
          s"so ignore this message and ban this peer.")
        self ! BanPeer(remote, SentPeersMessageWithoutRequest)

      case GetPeersNetworkMessage =>
        def findPeersForRemote(add: InetSocketAddress, info: PeerInfo): Boolean =
          Try {
            if (remote.socketAddress.getAddress.isSiteLocalAddress) true
            else add.getAddress.isSiteLocalAddress && add != remote.socketAddress
          }.getOrElse(false)

        val peers: Seq[InetSocketAddress] = connectedPeers.collect(findPeersForRemote, getPeersForRemote)
        logger.info(s"Got request for local known peers. Sending to: $remote peers: ${peers.mkString(",")}.")
        logger.info(s"Remote is side local: ${remote.socketAddress} : ${Try(remote.socketAddress.getAddress.isSiteLocalAddress)}")
        remote.handlerRef ! PeersNetworkMessage(peers)
    }
  }

  def additionalMessages(isBlockChainSynced: Boolean): Receive = {
    case OtherNodeSyncingStatus(remote, comparison, _) =>
      connectedPeers = connectedPeers.updateHistoryComparisonResult(Map(remote.socketAddress -> comparison))

    case AccumulatedPeersStatistic(statistic) =>
      connectedPeers = connectedPeers.updatePriorityStatus(statistic)

    case SendToNetwork(message, strategy) =>
      val peers: Seq[ConnectedPeer] = connectedPeers.collect(getAllPeers, getConnectedPeers)
      strategy.choose(peers).foreach { peer =>
        logger.debug(s"Sending message: ${message.messageName} to: ${peer.socketAddress}.")
        peer.handlerRef ! message
      }

    case SendLocalSyncInfo =>
      logger.debug(s"Received SendLocalSyncInfo from $sender on PK")
      val peersWithHP: Seq[ConnectedPeer] = connectedPeers.collect(filterByPriority(HighPriority), getConnectedPeers)
      val peersWithIP: Seq[ConnectedPeer] = connectedPeers.collect(filterByPriority(InitialPriority), getConnectedPeers)

      val accumulatedHPPeers = accumulatePeersForSync(peersWithHP, isBlockChainSynced)
      val accumulatedIPPeers = accumulatePeersForSync(peersWithIP, isBlockChainSynced)
      val accumulatedPeers = accumulatedHPPeers ++: accumulatedIPPeers

      accumulatedPeers.foreach { p =>
        logger.debug(s"Update uptime from $p")
        connectedPeers = connectedPeers.updateLastUptime(Map(p.socketAddress -> LastUptime(System.currentTimeMillis())))
      }
      nodeViewSync ! PeersForSyncInfo(accumulatedPeers)

      context.system.scheduler.scheduleOnce(settings.network.syncInterval) {
        logger.debug("Scheduler once for SendLocalSyncInfo triggered")
        self ! SendLocalSyncInfo
      }

    case PeerFromCli(peer) =>
      if (!blackList.contains(peer.getAddress) && !peersForConnection.contains(peer) && !connectedPeers.contains(peer) && !isSelf(peer)) {
        peersForConnection += (peer -> 0)
        knownPeers += peer.getAddress
        logger.info(s"Added peer: $peer to known peers. Current newPeers are: ${peersForConnection.mkString(",")}." +
          s" Current known peers are: ${knownPeers.mkString(",")}.")
      }

    case RemovePeerFromBlackList(peer) => blackList = blackList.remove(peer.getAddress)

    case FullBlockChainIsSynced =>
      logger.info(s"Peers keeper got message: FullBlockChainIsSynced")
      context.become(workingBehaviour(isBlockChainSynced = true))

    case msg => logger.info(s"Peers keeper got unhandled message: $msg.")
  }

  def banPeersLogic: Receive = {
    case BanPeer(peer, reason) =>
      logger.info(s"Banning peer: ${peer.socketAddress} for $reason.")
      blackList = blackList.banPeer(reason, peer.socketAddress.getAddress)
      peer.handlerRef ! CloseConnection

    case BanPeerFromAPI(peer, reason) =>
      logger.info(s"Got msg from API... Removing peer: $peer, reason: $reason")
      blackList = blackList.banPeer(reason, peer.getAddress)
  }

  //todo NPE in InetAddress.getLocalHost.getAddress.sameElements(address.getAddress.getAddress)
  def isSelf(address: InetSocketAddress): Boolean = Try(address == settings.network.bindAddress ||
    settings.network.declaredAddress.contains(address) ||
    InetAddress.getLocalHost.getAddress.sameElements(address.getAddress.getAddress) ||
    InetAddress.getLoopbackAddress.getAddress.sameElements(address.getAddress.getAddress)).getOrElse(true)

  def filterByPriority(priority: PeersPriorityStatus)(address: InetSocketAddress, info: PeerInfo): Boolean = {
    val isTimeRangeConserved: Boolean = (System.currentTimeMillis() - info.lastUptime.time) > settings.network.syncInterval.toMillis
    val isNecessaryPriority: Boolean = info.peerPriorityStatus == priority
    logger.debug(s"findByPriorityForSync: peer: $address, isTimeRangeConserved: $isTimeRangeConserved," +
      s" isNecessaryPriority: $isNecessaryPriority")
    isTimeRangeConserved && isNecessaryPriority
  }

  def getConnectedPeers(add: InetSocketAddress, info: PeerInfo): ConnectedPeer = info.connectedPeer

  def getPeersForRemote(add: InetSocketAddress, info: PeerInfo): InetSocketAddress = add

  def getPeersForDM(address: InetSocketAddress, info: PeerInfo): (InetSocketAddress, (ConnectedPeer, HistoryComparisonResult, PeersPriorityStatus)) =
    address -> (info.connectedPeer, info.historyComparisonResult, info.peerPriorityStatus)

  def getAllPeers: (InetSocketAddress, PeerInfo) => Boolean = (_, _) => true

  def accumulatePeersForSync(peers: Seq[ConnectedPeer], isChainSynced: Boolean): Seq[ConnectedPeer] = peers match {
    case coll: Seq[_] if coll.nonEmpty && isChainSynced =>
      logger.info(s"Peers collection for sync info non empty and block chain is synced. Sending to DM" +
        s" peers collection: ${coll.mkString(",")}.")
      coll
    case coll: Seq[_] if coll.nonEmpty => scala.util.Random.shuffle(coll).headOption.toSeq.map { p =>
      logger.info(s"Peers collection for sync info non empty but block chain is not synced. Sending to DM" +
        s" peer for sync: $p.")
      p
    }
    case _ =>
      logger.info(s"Peers collection for sync info message is empty.")
      Seq.empty[ConnectedPeer]

  }
}

object PeersKeeper {

  sealed trait PeerCommandHelper

  sealed trait ConnectionStatusMessages
  object ConnectionStatusMessages {
    final case class NewConnection(peer: InetSocketAddress,
                                   remoteConnection: ActorRef) extends ConnectionStatusMessages

    final case class ConnectionVerified(peer: InetSocketAddress,
                                        remoteConnection: ActorRef,
                                        ct: ConnectionType) extends ConnectionStatusMessages

    final case class OutgoingConnectionFailed(peer: InetSocketAddress) extends ConnectionStatusMessages

    final case class HandshakedDone(peer: ConnectedPeer) extends ConnectionStatusMessages

    final case class ConnectionStopped(peer: InetSocketAddress) extends ConnectionStatusMessages
  }

  case object RequestPeerForConnection

  final case class PeerForConnection(peer: InetSocketAddress)

  final case class SendToNetwork(message: NetworkMessage,
                                 sendingStrategy: SendingStrategy)

  case object RequestPeersForFirstSyncInfo

  final case class PeersForSyncInfo(peers: Seq[ConnectedPeer])

  final case class UpdatedPeersCollection(peers: Map[InetSocketAddress, (ConnectedPeer, HistoryComparisonResult, PeersPriorityStatus)])

  final case class BanPeer(peer: InetSocketAddress, reason: BanReason)

  final case class BanPeerFromAPI(peer: InetSocketAddress, reason: BanReason) extends PeerCommandHelper

  case object GetKnownPeers

  def props(settings: EncryAppSettings,
            nodeViewSync: ActorRef,
            dataHolder: ActorRef): Props = Props(new PeersKeeper(settings, nodeViewSync, dataHolder))

  class PeersKeeperPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case OtherNodeSyncingStatus(_, _, _) => 0
        case AccumulatedPeersStatistic(_)    => 1
        case BanPeer(_, _)                   => 1
        case SendLocalSyncInfo               => 1
        case NewConnection(_, _)          => 2
        case HandshakedDone(_)               => 2
        case ConnectionStopped(_)            => 2
        case OutgoingConnectionFailed(_)     => 2
        case PoisonPill                      => 4
        case _                               => 3
      })

}