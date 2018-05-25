package scorex.core.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface, URI}

import akka.actor._
import akka.io.Tcp.SO.KeepAlive
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import scorex.core.network.message.{Message, MessageHandler, MessageSpec}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import encry.EncryApp._
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.existentials
import scala.util.{Failure, Success, Try}
import scala.language.postfixOps

class NetworkController extends Actor with ScorexLogging {

  import NetworkController.ReceivableMessages._
  import NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
  import scorex.core.network.peer.PeerManager.ReceivableMessages.{CheckPeers, FilterPeers, Disconnected}
  import PeerConnectionHandler.ReceivableMessages.CloseConnection

  private implicit val system: ActorSystem = context.system

  val netSettings: NetworkSettings = settings.network

  private val peerSynchronizer: ActorRef = PeerSynchronizerRef("PeerSynchronizer", self, peerManager, netSettings)

  private implicit val timeout: Timeout = Timeout(netSettings.controllerTimeout.getOrElse(5 seconds))

  private val messageHandlers = mutable.Map[Seq[Message.MessageCode], ActorRef]()

  private val tcpManager = IO(Tcp)

  //check own declared address for validity
  if (!netSettings.localOnly) {
    netSettings.declaredAddress.foreach { myAddress =>
      Try {
        val uri = new URI("http://" + myAddress)
        val myHost = uri.getHost
        val myAddrs = InetAddress.getAllByName(myHost)

        NetworkInterface.getNetworkInterfaces.asScala.exists { intf =>
          intf.getInterfaceAddresses.asScala.exists { intfAddr =>
            val extAddr = intfAddr.getAddress
            myAddrs.contains(extAddr)
          }
        } || (netSettings.upnpEnabled && myAddrs.exists(_ == upnp.externalAddress))
      } recover { case t: Throwable =>
        log.error("Declared address validation failed: ", t)
      }
    }
  }

  lazy val localAddress: InetSocketAddress = netSettings.bindAddress

  //an address to send to peers
  lazy val externalSocketAddress: Option[InetSocketAddress] = {
    netSettings.declaredAddress orElse {
      if (netSettings.upnpEnabled) {
        upnp.externalAddress.map(a => new InetSocketAddress(a, netSettings.bindAddress.getPort))
      } else None
    }
  }

  log.info(s"Declared address: $externalSocketAddress")


  lazy val connTimeout = Some(netSettings.connectionTimeout)

  //bind to listen incoming connections
  tcpManager ! Bind(self, localAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  private def bindingLogic: Receive = {
    case Bound(_) =>
      log.info("Successfully bound to the port " + netSettings.bindAddress.getPort)
      context.system.scheduler.schedule(600.millis, 5.seconds)(peerManager ! CheckPeers)

    case CommandFailed(_: Bind) =>
      log.error("Network port " + netSettings.bindAddress.getPort + " already in use!")
      context stop self
    //TODO catch?
  }

  def businessLogic: Receive = {
    //a message coming in from another peer
    case Message(spec, Left(msgBytes), Some(remote)) =>
      val msgId = spec.messageCode

      spec.parseBytes(msgBytes) match {
        case Success(content) =>
          messageHandlers.find(_._1.contains(msgId)).map(_._2) match {
            case Some(handler) =>
              handler ! DataFromPeer(spec, content, remote)

            case None =>
              log.error("No handlers found for message: " + msgId)
            //todo: ban a peer
          }
        case Failure(e) =>
          log.error("Failed to deserialize data: ", e)
        //todo: ban peer
      }

    case SendToNetwork(message, sendingStrategy) =>
      (peerManager ? FilterPeers(sendingStrategy))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! message))
  }

  private val outgoing = mutable.Set[InetSocketAddress]()

  def peerLogic: Receive = {
    case ConnectTo(remote) =>
      log.info(s"Connecting to: $remote")
      outgoing += remote
      tcpManager ! Connect(remote,
        localAddress = externalSocketAddress,
        options = KeepAlive(true) :: Nil,
        timeout = connTimeout,
        pullMode = true) //todo: check pullMode flag

    case DisconnectFrom(peer) =>
      log.info(s"Disconnected from ${peer.socketAddress}")
      peer.handlerRef ! CloseConnection
      peerManager ! Disconnected(peer.socketAddress)

    case Blacklist(peer) =>
      peer.handlerRef ! PeerConnectionHandler.ReceivableMessages.Blacklist
      // todo: the following message might become unnecessary if we refactor PeerManager to automatically
      // todo: remove peer from `connectedPeers` on receiving `AddToBlackList` message.
      peerManager ! Disconnected(peer.socketAddress)

    case Connected(remote, local) =>
      val direction: ConnectionType = if (outgoing.contains(remote)) Outgoing else Incoming
      val logMsg = direction match {
        case Incoming => s"New incoming connection from $remote established (bound to local $local)"
        case Outgoing => s"New outgoing connection to $remote established (bound to local $local)"
      }
      log.info(logMsg)
      val connection = sender()
      val handlerProps: Props = PeerConnectionHandlerRef.props(netSettings, self, peerManager,
        messagesHandler, connection, direction, externalSocketAddress, remote, timeProvider)
      context.actorOf(handlerProps) // launch connection handler
      outgoing -= remote

    case CommandFailed(c: Connect) =>
      outgoing -= c.remoteAddress
      log.info("Failed to connect to : " + c.remoteAddress)
      peerManager ! Disconnected(c.remoteAddress)
  }

  //calls from API / application
  def interfaceCalls: Receive = {
    case ShutdownNetwork =>
      log.info("Going to shutdown all connections & unbind port")
      (peerManager ? FilterPeers(Broadcast))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! CloseConnection))
      self ! Unbind
      context stop self
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse interfaceCalls orElse {
    case RegisterMessagesHandler(specs, handler) =>
      log.info(s"Registering handlers for ${specs.map(s => s.messageCode -> s.messageName)}")
      messageHandlers += specs.map(_.messageCode) -> handler

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case nonsense: Any =>
      log.warn(s"NetworkController: got something strange $nonsense")
  }
}

object NetworkController {

  object ReceivableMessages {

    case class RegisterMessagesHandler(specs: Seq[MessageSpec[_]], handler: ActorRef)

    case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)

    case object ShutdownNetwork

    case class ConnectTo(address: InetSocketAddress)

    case class DisconnectFrom(peer: ConnectedPeer)

    case class Blacklist(peer: ConnectedPeer)

  }

}