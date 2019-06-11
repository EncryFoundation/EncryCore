package encry.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface, URI}
import akka.actor._
import akka.actor.SupervisorStrategy.Restart
import akka.io.{IO, Tcp}
import akka.io.Tcp._
import akka.io.Tcp.SO.KeepAlive
import com.typesafe.scalalogging.StrictLogging
import encry.network.BlackList.InvalidNetworkMessage
import encry.network.NetworkController.ReceivableMessages._
import encry.network.PeerConnectionHandler._
import encry.network.PeerConnectionHandler.ReceivableMessages.StartInteraction
import encry.network.PeersKeeper._
import encry.settings.EncryAppSettings
import org.encryfoundation.common.network.BasicMessagesRepo.NetworkMessage
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.language.{existentials, postfixOps}
import scala.util.Try

class NetworkController(settings: EncryAppSettings,
                        peersKeeper: ActorRef,
                        nodeViewSync: ActorRef) extends Actor with StrictLogging {

  import context.dispatcher
  import context.system

  var messagesHandlers: Map[Seq[Byte], ActorRef] = Map.empty
  val externalSocketAddress: Option[InetSocketAddress] = settings.network.declaredAddress
  logger.info(s"Declared address is: $externalSocketAddress.")

  if (!settings.network.localOnly.getOrElse(false)) settings.network.declaredAddress.foreach(myAddress =>
    Try(NetworkInterface.getNetworkInterfaces.asScala.exists(interface =>
      interface.getInterfaceAddresses.asScala.exists(interfaceAddress =>
        InetAddress.getAllByName(new URI("http://" + myAddress).getHost).contains(interfaceAddress.getAddress)
      ))).recover { case t: Throwable => logger.error(s"Declared address validation failed: $t") }
  )

  IO(Tcp) ! Bind(self, settings.network.bindAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(
    maxNrOfRetries = 5,
    withinTimeRange = 60 seconds) {
    case _ => Restart
  }

  override def receive: Receive = bindingLogic
    .orElse(businessLogic)
    .orElse(peersLogic)
    .orElse {
      case RegisterMessagesHandler(types, handler) =>
        logger.info(s"Registering handlers for ${types.mkString(",")}.")
        val ids = types.map(_._1)
        messagesHandlers += (ids -> handler)
      case CommandFailed(cmd: Tcp.Command) => logger.info(s"Failed to execute: $cmd.")
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
    case MessageFromNetwork(message, Some(remote)) if message.isValid(settings.network.syncPacketLength) =>
      logger.debug(s"Got ${message.messageName} on the NetworkController.")
      findHandler(message, message.NetworkMessageTypeID, remote, messagesHandlers)
    case MessageFromNetwork(message, Some(remote)) =>
      peersKeeper ! BanPeer(remote, InvalidNetworkMessage)
      logger.info(s"Invalid message type: ${message.messageName} from remote $remote.")
  }

  def peersLogic: Receive = {
    case PeerForConnection(peer) =>
      logger.info(s"Network controller got new peer for connection: $peer. Trying to set connection with remote...")
      IO(Tcp) ! Connect(
        peer,
        None,//externalSocketAddress,
        KeepAlive(true) :: Nil,
        Some(settings.network.connectionTimeout),
        pullMode = true
      )

    case Connected(remote, _) =>
      logger.info(s"Network controller got 'Connected' message from: $remote. Trying to set stable connection with remote...")
      peersKeeper ! VerifyConnection(remote, sender())

    case ConnectionVerified(remote, remoteConnection, connectionType) =>
      logger.info(s"Network controller got approvement for stable connection with: $remote. Starting interaction process...")
      val peerConnectionHandler: ActorRef = context.actorOf(
        PeerConnectionHandler.props(remoteConnection, connectionType, externalSocketAddress, remote)
          .withDispatcher("network-dispatcher")
      )
      peerConnectionHandler ! StartInteraction

    case HandshakedDone(remote) =>
      logger.info(s"Network controller got approvement from peer handler about successful handshake. " +
        s"Sending to peerKeeper connected peer.")
      peersKeeper ! HandshakedDone(remote)

    case ConnectionStopped(peer) =>
      logger.info(s"Network controller got signal about breaking connection with: $peer. " +
        s"Sending to peerKeeper actual information.")
      peersKeeper ! ConnectionStopped(peer)
      nodeViewSync ! ConnectionStopped(peer)

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
        handler ! DataFromPeer(message, remote)
        logger.debug(s"Send message DataFromPeer with ${message.messageName} to $handler.")
      case None => logger.error("No handlers found for message: " + message.messageName)
    }
}

object NetworkController {

  def props(settings: EncryAppSettings,
            peersKeeper: ActorRef,
            nodeViewSync: ActorRef): Props = Props(new NetworkController(settings, peersKeeper, nodeViewSync))

  object ReceivableMessages {

    case class DataFromPeer(message: NetworkMessage, source: ConnectedPeer)

    case class RegisterMessagesHandler(types: Seq[(Byte, String)], handler: ActorRef)
  }
}