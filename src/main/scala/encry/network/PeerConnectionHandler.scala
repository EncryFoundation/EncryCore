package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import com.google.common.primitives.Ints
import encry.EncryApp
import encry.network.PeerConnectionHandler.{AwaitingHandshake, CommunicationState, WorkingCycle}
import encry.EncryApp._
import scorex.core.app.Version
import scorex.core.network._
import scorex.core.network.message.MessageHandler
import scorex.core.settings.NetworkSettings
import scorex.core.utils.ScorexLogging

import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

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

class PeerConnectionHandler(messagesHandler: MessageHandler,
                            connection: ActorRef,
                            direction: ConnectionType,
                            ownSocketAddress: Option[InetSocketAddress],
                            remote: InetSocketAddress) extends Actor with Buffering with ScorexLogging {

  import PeerConnectionHandler.ReceivableMessages._
  import encry.network.peer.PeerManager.ReceivableMessages.{AddToBlacklist, Disconnected, DoConnecting, Handshaked}

  context watch connection

  override val settings: NetworkSettings = EncryApp.settings.network
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
      processErrors(AwaitingHandshake)

  def processErrors(stateName: CommunicationState): Receive = {
    case CommandFailed(w: Write) =>
      log.warn(s"Write failed :$w " + remote + s" in state $stateName")
      //      peerManager ! AddToBlacklist(remote)
      connection ! Close
      connection ! ResumeReading
      connection ! ResumeWriting
    case cc: ConnectionClosed =>
      peerManager ! Disconnected(remote)
      log.info("Connection closed to : " + remote + ": " + cc.getErrorCause + s" in state $stateName")
      context stop self
    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + remote + s" in state $stateName")
      connection ! Close
    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd + s" in state $stateName")
      connection ! ResumeReading
  }

  def startInteraction: Receive = {
    case StartInteraction =>
      val hb: Array[Byte] = Handshake(settings.agentName,
        Version(settings.appVersion), settings.nodeName,
        ownSocketAddress, timeProvider.time()).bytes
      connection ! Tcp.Write(ByteString(hb))
      log.info(s"Handshake sent to $remote")
      handshakeSent = true
      if (receivedHandshake.isDefined && handshakeSent) self ! HandshakeDone
  }

  def receivedData: Receive = {
    case Received(data) =>
      HandshakeSerializer.parseBytes(data.toArray) match {
        case Success(handshake) =>
          receivedHandshake = Some(handshake)
          log.info(s"Got a Handshake from $remote")
          connection ! ResumeReading
          if (receivedHandshake.isDefined && handshakeSent) self ! HandshakeDone
        case Failure(t) =>
          log.info(s"Error during parsing a handshake", t)
          self ! CloseConnection
      }
  }

  def handshakeTimeout: Receive = {
    case HandshakeTimeout =>
      log.info(s"Handshake timeout with $remote, going to drop the connection")
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
    case msg: message.Message[_] =>
      def sendOutMessage(): Unit = {
        val bytes: Array[Byte] = msg.bytes
        log.info("Send message " + msg.spec + " to " + remote)
        connection ! Write(ByteString(Ints.toByteArray(bytes.length) ++ bytes))
      }
      //simulating network delays
      settings.addedMaxDelay match {
        case Some(delay) =>
          context.system.scheduler.scheduleOnce(Random.nextInt(delay.toMillis.toInt).millis)(sendOutMessage())
        case None => sendOutMessage()
      }
    case Blacklist =>
      log.info(s"Going to blacklist " + remote)
      peerManager ! AddToBlacklist(remote)
      connection ! Close
  }

  def workingCycleRemoteInterface: Receive = {
    case Received(data) =>
      val t: (List[ByteString], ByteString) = getPacket(chunksBuffer ++ data)
      chunksBuffer = t._2
      t._1.find { packet =>
        messagesHandler.parseBytes(packet.toByteBuffer, selfPeer) match {
          case Success(message) =>
            log.info("Received message " + message.spec + " from " + remote)
            networkController ! message
            false
          case Failure(e) =>
            log.info(s"Corrupted data from: " + remote, e)
            //  connection ! Close
            //  context stop self
            true
        }
      }
      connection ! ResumeReading
  }

  def reportStrangeInput: Receive = {
    case nonsense: Any => log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
  }

  def workingCycle: Receive =
    workingCycleLocalInterface orElse
      workingCycleRemoteInterface orElse
      processErrors(WorkingCycle) orElse
      reportStrangeInput

  override def preStart: Unit = {
    peerManager ! DoConnecting(remote, direction)
    handshakeTimeoutCancellableOpt = Some(context.system.scheduler.scheduleOnce(settings.handshakeTimeout)
    (self ! HandshakeTimeout))
    connection ! Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)
    connection ! ResumeReading
  }

  override def postStop(): Unit = log.info(s"Peer handler to $remote destroyed")
}

object PeerConnectionHandler {

  // todo: use the "become" approach to handle state more elegantly
  sealed trait CommunicationState

  case object AwaitingHandshake extends CommunicationState

  case object WorkingCycle extends CommunicationState

  object ReceivableMessages {

    private[PeerConnectionHandler] object HandshakeDone

    case object StartInteraction

    case object HandshakeTimeout

    case object CloseConnection

    case object Blacklist

  }

  def props(messagesHandler: MessageHandler,
            connection: ActorRef,
            direction: ConnectionType,
            ownSocketAddress: Option[InetSocketAddress],
            remote: InetSocketAddress): Props =
    Props(new PeerConnectionHandler(messagesHandler, connection, direction, ownSocketAddress, remote))
}