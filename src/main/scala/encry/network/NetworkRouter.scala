package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp.{Bind, Bound, CommandFailed, Connect, Connected}
import akka.io.{IO, Tcp}
import akka.io.Tcp.SO.KeepAlive
import com.typesafe.scalalogging.StrictLogging
import encry.network.BlackList.BanReason.InvalidNetworkMessage
import encry.network.Messages.MessageToNetwork
import encry.network.MessageBuilder.{GetPeerInfo, GetPeerWithEqualHistory, GetPeerWithOlderHistory, GetPeers, MsgSent}
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.NetworkRouter.{ModifierFromNetwork, RegisterForModsHandling}
import encry.network.NodeViewSynchronizer.ReceivableMessages.OtherNodeSyncingStatus
import encry.network.PeerConnectionHandler.ReceivableMessages.StartInteraction
import encry.network.PeerConnectionHandler.{ConnectedPeer, MessageFromNetwork}
import encry.network.PeersKeeper.ConnectionStatusMessages.{ConnectionVerified, NewConnection, OutgoingConnectionFailed}
import encry.network.PeersKeeper.{BanPeer, ConnectionStatusMessages, PeerForConnection, RequestPeerForConnection}
import encry.settings.{BlackListSettings, NetworkSettings}
import org.encryfoundation.common.network.BasicMessagesRepo.NetworkMessage
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

import scala.concurrent.duration._
import scala.util.Random

class NetworkRouter(settings: NetworkSettings,
                    blackListSettings: BlackListSettings) extends Actor with StrictLogging {

  import context.system
  import context.dispatcher

  var messagesHandlers: Map[Seq[Byte], ActorRef] = Map.empty
  var handlerForMods: ActorRef = ActorRef.noSender

  IO(Tcp) ! Bind(self, settings.bindAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  val peersKeeper = context.actorOf(PK.props(settings, blackListSettings), "peersKeeper")
  val deliveryManager = context.actorOf(DM.props(settings), "deliveryManager")
  val externalSocketAddress: Option[InetSocketAddress] = settings.declaredAddress

  override def receive: Receive = bindingLogic orElse businessLogic orElse peersLogic orElse {
    case RegisterMessagesHandler(types, handler) =>
      logger.info(s"Registering handlers for ${types.mkString(",")}.")
      val ids = types.map(_._1)
      messagesHandlers += (ids -> handler)
    case msg: MsgSent => context.stop(sender())
    case CommandFailed(cmd: Tcp.Command) => logger.info(s"Failed to execute: $cmd.")
    case RegisterForModsHandling => handlerForMods = sender()
    case msg => logger.warn(s"NetworkController: got something strange $msg.")
  }

  def bindingLogic: Receive = {
    case Bound(address) =>
      logger.info(s"Successfully bound to the port ${address.getPort}.")
      context.system.scheduler.schedule(2.seconds, 5.second)(peersKeeper ! RequestPeerForConnection)
    case CommandFailed(add: Bind) =>
      logger.info(s"Node can't be bind to the address: ${add.localAddress}.")
      context.stop(self)
  }

  def businessLogic: Receive = {
    case MessageFromNetwork(message, Some(remote)) if message.isValid(settings.syncPacketLength) =>
      logger.debug(s"Got ${message.messageName} on the NetworkController.")
      findHandler(message, message.NetworkMessageTypeID, remote, messagesHandlers)
    case MessageFromNetwork(message, Some(remote)) =>
      peersKeeper ! BanPeer(remote.socketAddress, InvalidNetworkMessage(message.messageName))
      logger.info(s"Invalid message type: ${message.messageName} from remote $remote.")
    case msg: ModifierFromNetwork => handlerForMods ! msg
    case msg: OtherNodeSyncingStatus => peersKeeper ! msg
    case msg: MessageToNetwork =>
      context.actorOf(
        MessageBuilder.props(peersKeeper, deliveryManager),
        s"messageBuilder${Random.nextInt()}"
      ) ! msg
  }

  def peersLogic: Receive = {
    case PeerForConnection(peer) =>
      logger.info(s"Network router got new peer for connection: $peer. Trying to set connection with remote...")
      IO(Tcp) ! Connect(
        peer,
        None,
        KeepAlive(true) :: Nil,
        Some(settings.connectionTimeout),
        pullMode = true
      )
    case Connected(remote, localAddress) =>
      logger.info(s"Network controller got 'Connected' message from: $remote. " +
        s"Trying to set stable connection with remote... " +
        s"Local TCP endpoint is: $localAddress.")
      peersKeeper ! NewConnection(remote, sender())
    case ConnectionVerified(remote, remoteConnection, connectionType) =>
      logger.info(s"Network controller got approvement for stable connection with: $remote. Starting interaction process...")
      val peerConnectionHandler: ActorRef = context.actorOf(
        PeerConnectionHandler.props(remoteConnection, connectionType, externalSocketAddress, remote, settings)
          .withDispatcher("network-dispatcher")
      )
      peerConnectionHandler ! StartInteraction

    case msg: ConnectionStatusMessages => peersKeeper ! msg

    case CommandFailed(connect: Connect) =>
      logger.info(s"Failed to connect to: ${connect.remoteAddress}.")
      peersKeeper ! OutgoingConnectionFailed(connect.remoteAddress)
  }

  private def findHandler(message: NetworkMessage,
                          messageId: Byte,
                          remote: ConnectedPeer,
                          mH: Map[Seq[Byte], ActorRef]): Unit =
    mH.find(_._1.contains(messageId)).map(_._2) match {
      case Some(handler) =>
        handler ! DataFromPeer(message, remote.socketAddress)
        logger.debug(s"Send message DataFromPeer with ${message.messageName} to $handler.")
      case None => logger.info("No handlers found for message: " + message.messageName)
    }
}

object NetworkRouter {

  case class ModifierFromNetwork(source: InetSocketAddress,
                                 modTypeId: ModifierTypeId,
                                 modId: ModifierId,
                                 modBytes: Array[Byte])

  case object RegisterForModsHandling

  def props(settings: NetworkSettings,
            blackListSettings: BlackListSettings): Props = Props(new NetworkRouter(settings, blackListSettings))
}
