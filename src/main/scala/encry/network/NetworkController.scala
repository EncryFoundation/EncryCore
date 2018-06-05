package encry.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface, URI}

import akka.actor._
import akka.io.Tcp.SO.KeepAlive
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import encry.EncryApp._
import encry.view.history.EncrySyncInfoMessageSpec
import scorex.core.network._
import encry.network.message.Message.MessageCode
import encry.network.message.{Message, MessageHandler}
import encry.settings.NetworkSettings
import scorex.core.utils.ScorexLogging
import NetworkController.ReceivableMessages._
import PeerConnectionHandler.ReceivableMessages.CloseConnection
import PeerConnectionHandler._
import encry.network.peer.PeerManager.ReceivableMessages.{CheckPeers, Disconnected, FilterPeers}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}


class NetworkController extends Actor with ScorexLogging {

  val networkSettings: NetworkSettings = settings.network

  val peerSynchronizer: ActorRef = context.actorOf(Props[PeerSynchronizer], "peerSynchronizer")

  val tcpManager: ActorRef = IO(Tcp)

  implicit val timeout: Timeout = Timeout(networkSettings.controllerTimeout.getOrElse(5 seconds))

  val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ Seq(EncrySyncInfoMessageSpec))

  val messageHandlers: mutable.Map[Seq[MessageCode], ActorRef] = mutable.Map[Seq[Message.MessageCode], ActorRef]()

  val outgoing: mutable.Set[InetSocketAddress] = mutable.Set[InetSocketAddress]()

  lazy val externalSocketAddress: Option[InetSocketAddress] = networkSettings.declaredAddress orElse {
    if (networkSettings.upnpEnabled) upnp.externalAddress.map(a => new InetSocketAddress(a, networkSettings.bindAddress.getPort))
    else None
  }

  if (!networkSettings.localOnly) {
    networkSettings.declaredAddress.foreach { myAddress =>
      Try {
        val myAddrs: Array[InetAddress] = InetAddress.getAllByName(new URI("http://" + myAddress).getHost)
        NetworkInterface.getNetworkInterfaces.asScala.exists { intf =>
          intf.getInterfaceAddresses.asScala.exists { intfAddr => myAddrs.contains(intfAddr.getAddress) }
        } || (networkSettings.upnpEnabled && myAddrs.exists(_ == upnp.externalAddress))
      } recover { case t: Throwable =>
        log.error("Declared address validation failed: ", t)
      }
    }
  }

  log.info(s"Declared address: $externalSocketAddress")

  tcpManager ! Bind(self, networkSettings.bindAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  override def supervisorStrategy: SupervisorStrategy = commonSupervisorStrategy

  def bindingLogic: Receive = {
    case Bound(_) =>
      log.info("Successfully bound to the port " + networkSettings.bindAddress.getPort)
      context.system.scheduler.schedule(600.millis, 5.seconds)(peerManager ! CheckPeers)
    case CommandFailed(_: Bind) =>
      log.error("Network port " + networkSettings.bindAddress.getPort + " already in use!")
      context stop self
  }

  def businessLogic: Receive = {
    case Message(spec, Left(msgBytes), Some(remote)) =>
      val msgId: MessageCode = spec.messageCode
      spec.parseBytes(msgBytes) match {
        case Success(content) =>
          messageHandlers.find(_._1.contains(msgId)).map(_._2) match {
            case Some(handler) => handler ! DataFromPeer(spec, content, remote)
            case None => log.error("No handlers found for message: " + msgId)
          }
        case Failure(e) => log.error("Failed to deserialize data: ", e)
      }
    case SendToNetwork(message, sendingStrategy) =>
      (peerManager ? FilterPeers(sendingStrategy))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! message))
  }

  def peerLogic: Receive = {
    case ConnectTo(remote) =>
      log.info(s"Connecting to: $remote")
      outgoing += remote
      tcpManager ! Connect(remote,
        localAddress = externalSocketAddress,
        options = KeepAlive(true) :: Nil,
        timeout = Some(networkSettings.connectionTimeout),
        pullMode = true)
    case DisconnectFrom(peer) =>
      log.info(s"Disconnected from ${peer.socketAddress}")
      peer.handlerRef ! CloseConnection
      peerManager ! Disconnected(peer.socketAddress)
    case Blacklist(peer) =>
      peer.handlerRef ! PeerConnectionHandler.ReceivableMessages.Blacklist
      peerManager ! Disconnected(peer.socketAddress)
    case Connected(remote, local) =>
      val direction: ConnectionType = if (outgoing.contains(remote)) Outgoing else Incoming
      val logMsg: String = direction match {
        case Incoming => s"New incoming connection from $remote established (bound to local $local)"
        case Outgoing => s"New outgoing connection to $remote established (bound to local $local)"
      }
      log.info(logMsg)
      context.actorOf(PeerConnectionHandler.props(messagesHandler, sender(), direction, externalSocketAddress, remote))
      outgoing -= remote
    case CommandFailed(c: Connect) =>
      outgoing -= c.remoteAddress
      log.info("Failed to connect to : " + c.remoteAddress)
      peerManager ! Disconnected(c.remoteAddress)
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse {
    case RegisterMessagesHandler(specs, handler) =>
      log.info(s"Registering handlers for ${specs.map(s => s.messageCode -> s.messageName)}")
      messageHandlers += specs.map(_.messageCode) -> handler
    case CommandFailed(cmd: Tcp.Command) => log.info("Failed to execute command : " + cmd)
    case nonsense: Any => log.warn(s"NetworkController: got something strange $nonsense")
    case ShutdownNetwork =>
      log.info("Going to shutdown all connections & unbind port")
      (peerManager ? FilterPeers(Broadcast))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! CloseConnection))
      self ! Unbind
      context stop self
  }
}

object NetworkController {

  object ReceivableMessages {

    import encry.network.message.MessageSpec
    import scala.reflect.runtime.universe.TypeTag

    case class DataFromPeer[DT: TypeTag](spec: MessageSpec[DT], data: DT, source: ConnectedPeer)

    case class RegisterMessagesHandler(specs: Seq[MessageSpec[_]], handler: ActorRef)

    case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)

    case object ShutdownNetwork

    case class ConnectTo(address: InetSocketAddress)

    case class DisconnectFrom(peer: ConnectedPeer)

    case class Blacklist(peer: ConnectedPeer)

  }

}
