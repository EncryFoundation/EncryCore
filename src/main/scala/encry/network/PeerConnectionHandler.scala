package encry.network

import java.net.InetSocketAddress
import java.nio.ByteOrder

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import com.google.common.primitives.Ints
import encry.EncryApp
import encry.EncryApp._
import encry.network.NetworkController.ReceivableMessages.ConnectTo
import encry.network.PeerConnectionHandler.{AwaitingHandshake, CommunicationState, WorkingCycle, _}
import encry.network.message.MessageHandler
import encry.settings.NetworkSettings
import encry.utils.Logging
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

class PeerConnectionHandler(messagesHandler: MessageHandler,
                            connection: ActorRef,
                            direction: ConnectionType,
                            ownSocketAddress: Option[InetSocketAddress],
                            remote: InetSocketAddress) extends Actor with Logging {

  import PeerConnectionHandler.ReceivableMessages._
  import encry.network.peer.PeerManager.ReceivableMessages.{Disconnected, DoConnecting, Handshaked}

  context watch connection

  val settings: NetworkSettings = EncryApp.settings.network
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
      logWarn(s"Write failed :$w " + remote + s" in state $stateName")
      connection ! Close
      connection ! ResumeReading
      connection ! ResumeWriting
    case cc: ConnectionClosed =>
      log.info("Connection closed to : " + remote + ": " + cc.getErrorCause + s" in state $stateName")
      peerManager ! Disconnected(remote)
      networkController ! ConnectTo(remote)
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
      log.info(s"Handshake sent to $remote")
      val hb: Array[Byte] = Handshake(Version(settings.appVersion), settings.nodeName,
        ownSocketAddress, timeProvider.time()).bytes
      connection ! Tcp.Write(ByteString(hb))
      handshakeSent = true
      if (receivedHandshake.isDefined && handshakeSent) self ! HandshakeDone
  }

  def receivedData: Receive = {
    case Received(data) =>
      HandshakeSerializer.parseBytes(data.toArray) match {
        case Success(handshake) =>
          log.info(s"Got a Handshake from $remote")
          receivedHandshake = Some(handshake)
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
        log.info("Send message " + msg.spec + " to " + remote)
        connection ! Write(ByteString(Ints.toByteArray(msg.bytes.length) ++ msg.bytes))
      }

      settings.addedMaxDelay match {
        case Some(delay) => context.system.scheduler.scheduleOnce(Random.nextInt(delay.toMillis.toInt).millis)(sendOutMessage())
        case None => sendOutMessage()
      }
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
            true
        }
      }
      connection ! ResumeReading
  }

  def reportStrangeInput: Receive = {
    case nonsense: Any => logWarn(s"Strange input for PeerConnectionHandler: $nonsense")
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

  override def postStop(): Unit = {
    log.info(s"Peer handler to $remote is destroyed")
    networkController ! ConnectTo(remote)
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