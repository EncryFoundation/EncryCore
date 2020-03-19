package encry.network

import java.net.{InetAddress, InetSocketAddress}
import java.nio.ByteOrder

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.timeProvider
import encry.network.PeerConnectionHandler.{AwaitingHandshake, CommunicationState, _}
import encry.network.PeerConnectionHandler.ReceivableMessages._
import encry.network.PeersKeeper.ConnectionStatusMessages.{ConnectionStopped, HandshakedDone}
import encry.settings.NetworkSettings
import org.encryfoundation.common.network.BasicMessagesRepo.{GeneralizedNetworkMessage, Handshake, NetworkMessage}
import org.encryfoundation.common.utils.Algos

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, SortedMap}
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

class PeerConnectionHandler(connection: ActorRef,
                            direction: ConnectionType,
                            ownSocketAddress: Option[InetSocketAddress],
                            remote: InetSocketAddress,
                            networkSettings: NetworkSettings) extends Actor with StrictLogging {

  context watch connection

  implicit val exCon: ExecutionContextExecutor = context.dispatcher

  var receivedHandshake: Option[Handshake] = None
  var selfPeer: Option[ConnectedPeer] = None
  var handshakeSent = false
  var handshakeTimeoutCancellableOpt: Option[Cancellable] = None
  var chunksBuffer: ByteString = CompactByteString.empty
  var outMessagesBuffer: SortedMap[Long, ByteString] = SortedMap.empty[Long, ByteString]
  var outMessagesCounter: Long = 0
  var sendResumeWriting: Boolean = false

  override def preStart: Unit = {
    handshakeTimeoutCancellableOpt = Some(
      context.system.scheduler.scheduleOnce(networkSettings.handshakeTimeout)(self ! HandshakeTimeout)
    )
    connection ! Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)
    connection ! ResumeReading
  }

  override def postStop(): Unit = {
    logger.info(s"Peer handler $self to $remote is destroyed.")
    connection ! Close
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit =
    logger.info(s"Reason of restarting actor $self: ${reason.toString}.")

  override def receive: Receive = startInteraction
    .orElse(receivedData)
    .orElse(handshakeTimeout)
    .orElse(handshakeDone)
    .orElse(processErrors(AwaitingHandshake))

  def startInteraction: Receive = {
    case StartInteraction =>
      timeProvider.time().map { time =>
        val handshake: Handshake = Handshake(
          protocolToBytes(networkSettings.appVersion),
          networkSettings.nodeName
            .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + networkSettings.bindAddress.getPort),
          ownSocketAddress,
          time
        )
        connection ! Tcp.Write(ByteString(GeneralizedNetworkMessage.toProto(handshake).toByteArray))
        logger.info(s"Handshake sent to $remote.")
        handshakeSent = true
        if (receivedHandshake.isDefined && handshakeSent) self ! HandshakeDone
      }
  }

  def receivedData: Receive = {
    case Received(data) => GeneralizedNetworkMessage.fromProto(data.toArray) match {
      case Success(value) => value match {
        case handshake: Handshake =>
          logger.info(s"Got a Handshake from $remote.")
          receivedHandshake = Some(handshake)
          connection ! ResumeReading
          if (receivedHandshake.isDefined && handshakeSent) self ! HandshakeDone
        case message => logger.info(s"Have expecting handshake, but received ${message.messageName}.")
      }
      case Failure(exception) =>
        logger.info(s"Error during parsing a handshake: $exception.")
        self ! CloseConnection
    }
  }

  def handshakeTimeout: Receive = {
    case HandshakeTimeout =>
      logger.info(s"Handshake timeout with $remote, going to drop the connection.")
      self ! CloseConnection
  }

  def handshakeDone: Receive = {
    case HandshakeDone =>
      require(receivedHandshake.isDefined)
      logger.info(s"Got handshake with declared address: ${receivedHandshake.get.declaredAddress}")
      val peer: ConnectedPeer = ConnectedPeer(
        receivedHandshake.get.declaredAddress.getOrElse(remote),
        self,
        direction,
        receivedHandshake.get
      )
      selfPeer = Some(peer)
      context.parent ! HandshakedDone(peer)
      handshakeTimeoutCancellableOpt.map(_.cancel())
      connection ! ResumeReading
      logger.debug(s"Starting workingCycleWriting on peerHandler for $remote.")
      context.become(workingCycleWriting)
  }

  def processErrors(stateName: CommunicationState): Receive = {
    case cc: ConnectionClosed if stateName == AwaitingHandshake =>
      logger.info("Connection closed to : " + remote + ": " + cc.getErrorCause + s" in state $stateName")
      context.parent ! ConnectionStopped(remote)
      context.stop(self)

    case cc: ConnectionClosed =>
      logger.info("Connection closed to : " + remote + ": " + cc.getErrorCause + s" in state $stateName")
      context.parent ! ConnectionStopped(receivedHandshake.get.declaredAddress.getOrElse(remote))
      context.stop(self)

    case CloseConnection =>
      logger.info(s"Enforced to abort communication with: " + remote + s" in state $stateName")
      connection ! Close

    case fail@CommandFailed(cmd: Tcp.Command) =>
      logger.debug("Failed to execute command : " + cmd + s" in state $stateName cause ${fail.cause}")
      connection ! ResumeReading

    case message => logger.info(s"Peer connection handler for $remote Got something strange: $message")
  }

  def workingCycleWriting: Receive =
    workingCycleLocalInterfaceWritingMode orElse
      workingCycleRemoteInterface orElse
      processErrors(WorkingCycleWriting)

  def workingCycleBuffering: Receive =
    workingCycleLocalInterfaceBufferingMode orElse
      workingCycleRemoteInterface orElse
      processErrors(WorkingCycleBuffering)

  def workingCycleLocalInterfaceWritingMode: Receive = {
    case message: NetworkMessage =>
      def sendMessage(): Unit = {
        outMessagesCounter += 1
         val messageToNetwork: Array[Byte] = GeneralizedNetworkMessage.toProto(message).toByteArray
        val bytes: ByteString = ByteString(Ints.toByteArray(messageToNetwork.length) ++ messageToNetwork)
        logger.debug(s"Sent to $remote msg: ${message.messageName}. outMessagesCounter = $outMessagesCounter. " +
          s"Msg hash: ${Algos.encode(Algos.hash(ByteString(Ints.toByteArray(messageToNetwork.length) ++ messageToNetwork).toArray))}")
        connection ! Write(bytes, Ack(outMessagesCounter))
      }
      networkSettings.addedMaxDelay match {
        case Some(delay) =>
          context.system.scheduler.scheduleOnce(Random.nextInt(delay.toMillis.toInt).millis)(sendMessage())
        case None => sendMessage()
      }
    case fail@CommandFailed(Write(msg, Ack(id))) =>
      logger.debug(s"Failed to write msg with hash ${Algos.encode(Algos.hash(msg.toArray))}, ack = $id, ${msg.length} bytes to " +
        s"$remote cause ${fail.cause}, switching to buffering mode")
      connection ! ResumeReading
      connection ! ResumeWriting
      sendResumeWriting = true
      toBuffer(id, msg)
      context.become(workingCycleBuffering)
    case CloseConnection =>
      logger.info(s"Enforced to abort communication with: " + remote + ", switching to closing mode")
      if (outMessagesBuffer.isEmpty) connection ! Close else context.become(workingCycleClosingWithNonEmptyBuffer)
    case Ack(_) => // ignore ACKs in stable mode
    case WritingResumed => // ignore in stable mode
  }

  // operate in ACK mode until all buffered messages are transmitted
  def workingCycleLocalInterfaceBufferingMode: Receive = {
    case message: NetworkMessage =>
      outMessagesCounter += 1
      val messageToNetwork: Array[Byte] = GeneralizedNetworkMessage.toProto(message).toByteArray
      val bytes: ByteString = ByteString(Ints.toByteArray(messageToNetwork.length) ++ messageToNetwork)
      toBuffer(outMessagesCounter, bytes)
    case fail@CommandFailed(Write(msg, Ack(id))) =>
      logger.debug(s"Failed to buffer msg hash: ${Algos.encode(Algos.hash(msg.toArray))}, with id: ${id}, ${msg.length} bytes to $remote cause ${fail.cause}")
      if (!sendResumeWriting) {
        sendResumeWriting = true
        connection ! ResumeWriting
      }
      toBuffer(id, msg)
    case CommandFailed(ResumeWriting) => // ignore in ACK mode
    case WritingResumed => {
      sendResumeWriting = false
      writeFirst()
    }
    case Ack(id) =>
      outMessagesBuffer -= id
      if (outMessagesBuffer.nonEmpty) writeFirst()
      else {
        logger.debug("Buffered messages processed, exiting buffering mode")
        context become workingCycleWriting
      }
    case CloseConnection =>
      logger.info(s"Enforced to abort communication with: " + remote + s", switching to closing mode")
      writeAll()
      context become workingCycleClosingWithNonEmptyBuffer
  }

  def workingCycleClosingWithNonEmptyBuffer: Receive = {
    case CommandFailed(_: Write) =>
      logger.info("CommandFailed in workingCycleClosingWithNonEmptyBuffer")
      connection ! ResumeWriting
      context.become({
        case WritingResumed =>
          writeAll()
          context.unbecome()
        case Ack(id) => outMessagesBuffer -= id
      }, discardOld = false)
    case Ack(id) =>
      outMessagesBuffer -= id
      if (outMessagesBuffer.isEmpty) connection ! Close
    case message => logger.debug(s"Got strange message $message during closing phase")
  }

  def writeFirst(): Unit = {
    outMessagesBuffer.headOption.foreach { case (id, msg) =>
      logger.debug(s"Write to connection of handler $remote msg with id: ${id} and hash: ${Algos.encode(Algos.hash(msg.toArray))}")
      connection ! Write(msg, Ack(id))
      outMessagesBuffer -= id
    }
  }

  def writeAll(): Unit = outMessagesBuffer.foreach { case (id, msg) => connection ! Write(msg, Ack(id)) }

  def toBuffer(id: Long, message: ByteString): Unit = outMessagesBuffer += id -> message

  def workingCycleRemoteInterface: Receive = {
    case Received(data) =>
      val packet: (List[ByteString], ByteString) = getPacket(chunksBuffer ++ data)
      chunksBuffer = packet._2
      packet._1.find { packet =>
        GeneralizedNetworkMessage.fromProto(packet.toArray) match {
          case Success(message) =>
            context.parent ! MessageFromNetwork(message, selfPeer)
            logger.debug("Received message " + message.messageName + " from " + remote)
            false
          case Failure(e) =>
            logger.debug(s"Corrupted data from: " + remote + s"$e")
            true
        }
      }
      connection ! ResumeReading
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

  case object WorkingCycleWriting extends CommunicationState

  case object WorkingCycleBuffering extends CommunicationState

  object ReceivableMessages {

    case object HandshakeDone

    case object StartInteraction

    case object HandshakeTimeout

    case object CloseConnection

    final case class Ack(offset: Long) extends Tcp.Event

  }

  /**
    * @param message - message, received from network
    * @param source - sender of received message
    *
    * This case class transfers network message from PeerConnectionHandler actor to the NetworkController.
    * Main duty is to transfer message from network with sender of it message to the NetworkController as an end point.
    */

  final case class MessageFromNetwork(message: NetworkMessage, source: Option[ConnectedPeer])

  def props(connection: ActorRef, direction: ConnectionType,
            ownSocketAddress: Option[InetSocketAddress], remote: InetSocketAddress, networkSettings: NetworkSettings): Props =
    Props(new PeerConnectionHandler(connection, direction, ownSocketAddress, remote, networkSettings))
}