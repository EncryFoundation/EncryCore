package encry.network

import java.net.{InetAddress, InetSocketAddress}
import java.nio.ByteOrder

import NetworkMessagesProto.HandshakeProtoMessage
import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import com.google.common.primitives.Ints
import encry.EncryApp.settings
import encry.EncryApp._
import encry.network.PeerConnectionHandler.{AwaitingHandshake, CommunicationState, WorkingCycle, _}
import encry.network.message.{Message, Message => _, _}
import PeerManager.ReceivableMessages.{Disconnected, DoConnecting, Handshaked}
import SyntaxMessageProto.InetSocketAddressProtoMessage
import com.typesafe.scalalogging.StrictLogging
import com.google.protobuf.{ByteString => ProtoBytes}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

class PeerConnectionHandler(messagesHandler: MessageHandler,
                            connection: ActorRef,
                            direction: ConnectionType,
                            ownSocketAddress: Option[InetSocketAddress],
                            remote: InetSocketAddress) extends Actor with StrictLogging {

  import PeerConnectionHandler.ReceivableMessages._

  context watch connection

  var receivedHandshake: Option[Handshake] = None
  var selfPeer: Option[ConnectedPeer] = None
  var handshakeSent = false
  var handshakeTimeoutCancellableOpt: Option[Cancellable] = None
  var chunksBuffer: ByteString = CompactByteString()

  override def receive: Receive =
    startInteraction orElse
      receivedData orElse
      handshakeTimeout orElse
      handshakeDone orElse
      processErrors(AwaitingHandshake) orElse deadNotIn

  def processErrors(stateName: CommunicationState): Receive = {
    case CommandFailed(w: Write) =>
      logger.warn(s"Write failed :$w " + remote + s" in state $stateName")
      connection ! Close
      connection ! ResumeReading
      connection ! ResumeWriting
    case cc: ConnectionClosed =>
      logger.info("Connection closed to : " + remote + ": " + cc.getErrorCause + s" in state $stateName")
      peerManager ! Disconnected(remote)
      context stop self
    case CloseConnection =>
      logger.info(s"Enforced to abort communication with: " + remote + s" in state $stateName")
      connection ! Close
    case CommandFailed(cmd: Tcp.Command) =>
      logger.info("Failed to execute command : " + cmd + s" in state $stateName")
      connection ! ResumeReading
  }

  def startInteraction: Receive = {
    case StartInteraction =>
      timeProvider.time().map { time =>
        val handshake: Handshake = Handshake(protocolToBytes(settings.network.appVersion),
          settings.network.nodeName
            .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort),
          ownSocketAddress, time
        )
        connection ! Tcp.Write(serialize(handshake))
        logger.info(s"Handshake sent to $remote")
        handshakeSent = true
        if (receivedHandshake.isDefined && handshakeSent) self ! HandshakeDone
      }
  }

  def receivedData: Receive = {
    case Received(data) => deserialize(data) match {
      case Success(handshake: Handshake) =>
        logger.info(s"Got a Handshake from $remote")
        receivedHandshake = Some(handshake)
        connection ! ResumeReading
        if (receivedHandshake.isDefined && handshakeSent) self ! HandshakeDone
      case Failure(t) =>
        logger.info(s"Error during parsing a handshake: $t")
        self ! CloseConnection
    }
  }

  def handshakeTimeout: Receive = {
    case HandshakeTimeout =>
      logger.info(s"Handshake timeout with $remote, going to drop the connection")
      self ! CloseConnection
  }

  def handshakeDone: Receive = {
    case HandshakeDone =>
      require(receivedHandshake.isDefined)
      val peer: ConnectedPeer = ConnectedPeer(remote, self, direction, receivedHandshake.get)
      selfPeer = Some(peer)
      peerManager ! Handshaked(peer)
      handshakeTimeoutCancellableOpt.map(_.cancel())
      connection ! ResumeReading
      context become workingCycle
  }

  def workingCycleLocalInterface: Receive = {
    case message: NetworkMessage =>
      def sendOutMessage(): Unit = {
        logger.info("Send message " + message.messageName + " to " + remote)
        connection ! Write(serialize(message))
      }

      settings.network.addedMaxDelay match {
        case Some(delay) =>
          context.system.scheduler.scheduleOnce(Random.nextInt(delay.toMillis.toInt).millis)(sendOutMessage())
        case None => sendOutMessage()
      }
  }

  def workingCycleRemoteInterface: Receive = {
    case Received(data) =>
      val packet: (List[ByteString], ByteString) = getPacket(chunksBuffer ++ data)
      chunksBuffer = packet._2
      packet._1.find { packet =>
        deserialize(packet) match {
          case Success(message) =>
            logger.info("Received message " + message.messageName + " from " + remote)
            networkController ! Message(message, selfPeer)
            false
          case Failure(e) =>
            logger.info(s"Corrupted data from: " + remote + s"$e")
            true
        }
      }
      connection ! ResumeReading
  }

  def reportStrangeInput: Receive = {
    case nonsense: Any => logger.warn(s"Strange input for PeerConnectionHandler: $nonsense")
  }

  def workingCycle: Receive =
    workingCycleLocalInterface orElse
      workingCycleRemoteInterface orElse
      processErrors(WorkingCycle) orElse
      reportStrangeInput orElse dead

  override def preStart: Unit = {
    peerManager ! DoConnecting(remote, direction)
    handshakeTimeoutCancellableOpt = Some(context.system.scheduler.scheduleOnce(settings.network.handshakeTimeout)
    (self ! HandshakeTimeout))
    connection ! Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)
    connection ! ResumeReading
  }

  def dead: Receive = {
    case message => logger.debug(s"Got smth strange: $message")
  }

  def deadNotIn: Receive = {
    case message => logger.debug(s"Got smth node strange: $message")
  }

  override def postStop(): Unit = {
    logger.info(s"Peer handler $self to $remote is destroyed.")
    connection ! Close
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    logger.info(s"Reason of restarting actor $self: ${reason.toString}.")
  }

  def getPacket(data: ByteString): (List[ByteString], ByteString) = {

    val headerSize: Int = 4

    @tailrec
    def multiPacket(packets: List[ByteString], current: ByteString): (List[ByteString], ByteString) =
      if (current.length < headerSize) (packets.reverse, current)
      else {
        val len: Int = current.iterator.getInt(ByteOrder.BIG_ENDIAN)
        if (current.length < len + headerSize) (packets.reverse, current)
        else {
          val rem: ByteString = current drop headerSize
          val (front: ByteString, back: ByteString) = rem.splitAt(len)
          multiPacket(front :: packets, back)
        }
      }

    multiPacket(List[ByteString](), data)
  }

  def serialize(message: NetworkMessage): ByteString = message match {
    case handshake: Handshake => NetworkMessagesIds.HandShake +: Handshake.toProto(handshake)
    case syncInfo: SyncInfoNetworkMessage => NetworkMessagesIds.SyncInfo +: SyncInfoNetworkMessage.toProto(syncInfo)
    case inv: InvNetworkMessage => NetworkMessagesIds.Inv +: InvNetworkMessage.toProto(inv)
    case requestModifier: RequestModifiersNetworkMessage =>
      NetworkMessagesIds.RequestModifier +: RequestModifiersNetworkMessage.toProto(requestModifier)
    case modifiers: ModifiersNetworkMessage => NetworkMessagesIds.Modifier +: ModifiersNetworkMessage.toProto(modifiers)
    case _: GetPeersNetworkMessage => NetworkMessagesIds.GetPeers +: GetPeersNetworkMessage.toProto
    case peers: PeersNetworkMessage => NetworkMessagesIds.Peers +: PeersNetworkMessage.toProto(peers)
  }

  def deserialize(bytes: ByteString): Try[NetworkMessage] = bytes.head match {
    case NetworkMessagesIds.HandShake => Handshake.fromProto(bytes.tail)
    case NetworkMessagesIds.SyncInfo => SyncInfoNetworkMessage.fromProto(bytes.tail)
    case NetworkMessagesIds.Inv => InvNetworkMessage.fromProto(bytes.tail)
    case NetworkMessagesIds.RequestModifier => RequestModifiersNetworkMessage.fromProto(bytes.tail)
    case NetworkMessagesIds.Modifier => ModifiersNetworkMessage.fromProto(bytes.tail)
    case NetworkMessagesIds.GetPeers => GetPeersNetworkMessage.fromProto(bytes.tail)
    case NetworkMessagesIds.Peers => PeersNetworkMessage.fromProto(bytes.tail)
    case wrongType => throw new Exception(s"There is no such deserializer for type: $wrongType!")
  }

  private def protocolToBytes(protocol: String): Array[Byte] = protocol.split("\\.").map(elem => elem.toByte)
}

object PeerConnectionHandler {

  sealed trait ConnectionType

  case object Incoming extends ConnectionType

  case object Outgoing extends ConnectionType

  case class ConnectedPeer(socketAddress: InetSocketAddress,
                           handlerRef: ActorRef,
                           direction: ConnectionType,
                           handshake: Handshake) {

    import shapeless.syntax.typeable._

    def publicPeer: Boolean = handshake.declaredAddress.contains(socketAddress)

    override def hashCode(): Int = socketAddress.hashCode()

    override def equals(obj: Any): Boolean =
      obj.cast[ConnectedPeer].exists(p => p.socketAddress == this.socketAddress && p.direction == this.direction)

    override def toString: String = s"ConnectedPeer($socketAddress)"
  }

  sealed trait CommunicationState

  case object AwaitingHandshake extends CommunicationState

  case object WorkingCycle extends CommunicationState

  object ReceivableMessages {

    case object HandshakeDone

    case object StartInteraction

    case object HandshakeTimeout

    case object CloseConnection

  }

  def props(messagesHandler: MessageHandler, connection: ActorRef, direction: ConnectionType,
            ownSocketAddress: Option[InetSocketAddress], remote: InetSocketAddress): Props =
    Props(new PeerConnectionHandler(messagesHandler, connection, direction, ownSocketAddress, remote))
}