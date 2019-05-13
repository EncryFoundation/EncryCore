package encry.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface, URI}
import akka.actor._
import akka.io.Tcp.SO.KeepAlive
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import encry.EncryApp._
import encry.network.NetworkController.ReceivableMessages._
import encry.network.PeerConnectionHandler._
import PeerManager.ReceivableMessages.{CheckPeers, Disconnected, FilterPeers}
import com.typesafe.scalalogging.StrictLogging
import encry.cli.commands.AddPeer.PeerFromCli
import BasicMessagesRepo._
import encry.settings.NetworkSettings
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.language.{existentials, postfixOps}
import scala.util.Try

class NetworkController extends Actor with StrictLogging {

  val networkSettings: NetworkSettings = settings.network
  val peerSynchronizer: ActorRef =
    context.actorOf(Props[PeerSynchronizer].withDispatcher("network-dispatcher"), "peerSynchronizer")
  var messagesHandlers: Map[Seq[Byte], ActorRef] = Map.empty
  var outgoing: Set[InetSocketAddress] = Set.empty
  lazy val externalSocketAddress: Option[InetSocketAddress] = networkSettings.declaredAddress orElse None
  var knownPeersCollection: Set[InetSocketAddress] = settings.network.knownPeers.toSet

  if (!networkSettings.localOnly.getOrElse(false)) {
    networkSettings.declaredAddress.foreach { myAddress =>
      Try {
        val myAddrs: Array[InetAddress] = InetAddress.getAllByName(new URI("http://" + myAddress).getHost)
        NetworkInterface.getNetworkInterfaces.asScala.exists { intf =>
          intf.getInterfaceAddresses.asScala.exists { intfAddr => myAddrs.contains(intfAddr.getAddress) }
        }
      } recover { case t: Throwable => logger.error(s"Declared address validation failed: $t") }
    }
  }

  logger.info(s"Declared address: $externalSocketAddress")

  IO(Tcp) ! Bind(self, networkSettings.bindAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  override def supervisorStrategy: SupervisorStrategy = commonSupervisorStrategy

  def bindingLogic: Receive = {
    case Bound(_) =>
      logger.info("Successfully bound to the port " + networkSettings.bindAddress.getPort)
      context.system.scheduler.schedule(600.millis, 5.seconds)(peerManager ! CheckPeers)
    case CommandFailed(_: Bind) =>
      logger.info("Network port " + networkSettings.bindAddress.getPort + " already in use!")
      context stop self
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

  def businessLogic: Receive = {
    case MessageFromNetwork(message, Some(remote)) if message.isValid(settings) =>
      logger.debug(s"Got ${message.messageName} on the NetworkController.")
      findHandler(message, message.NetworkMessageTypeID, remote, messagesHandlers)
    case MessageFromNetwork(message, Some(remote)) =>
      logger.info(s"Invalid message type: ${message.messageName} from remote $remote")
    case SendToNetwork(message, sendingStrategy) => {
      logger.info(s"SendToNetwork! ${message} by strategy ${sendingStrategy}")
      (peerManager ? FilterPeers(sendingStrategy)) (5 seconds)
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach{peer =>
          logger.info(s"Send to $peer msg $message")
          peer.handlerRef ! message
        }
        )
    }
  }

  def peerLogic: Receive = {
    ///TODO: Duplicate `checkPossibilityToAddPeer` logic
    case ConnectTo(remote) if CheckPeersObj.checkPossibilityToAddPeer(remote, knownPeersCollection, settings) =>
      outgoing += remote
      IO(Tcp) ! Connect(remote,
        localAddress = externalSocketAddress,
        options = KeepAlive(true) :: Nil,
        timeout = Some(networkSettings.connectionTimeout),
        pullMode = true)
    case PeerFromCli(address) =>
      knownPeersCollection = knownPeersCollection + address
      peerManager ! PeerFromCli(address)
      peerSynchronizer ! PeerFromCli(address)
      self ! ConnectTo(address)
    case Connected(remote, local) if CheckPeersObj.checkPossibilityToAddPeer(remote, knownPeersCollection, settings) =>
      val direction: ConnectionType = if (outgoing.contains(remote)) Outgoing else Incoming
      val logMsg: String = direction match {
        case Incoming => s"New incoming connection from $remote established (bound to local $local)"
        case Outgoing => s"New outgoing connection to $remote established (bound to local $local)"
      }
      logger.info(logMsg)
      context.actorOf(PeerConnectionHandler.props(sender(), direction, externalSocketAddress, remote)
        .withDispatcher("network-dispatcher"))
      outgoing -= remote
    case Connected(remote, _) =>
      logger.info(s"Peer $remote trying to connect, but checkPossibilityToAddPeer(remote):" +
        s" ${CheckPeersObj.checkPossibilityToAddPeer(remote, knownPeersCollection, settings)}.")
    case CommandFailed(c: Connect) =>
      outgoing -= c.remoteAddress
      logger.info("Failed to connect to : " + c.remoteAddress)
      peerManager ! Disconnected(c.remoteAddress)
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse {
    case RegisterMessagesHandler(types, handler) =>
      logger.info(s"Registering handlers for ${types.mkString(",")}.")
      val ids = types.map(_._1)
      messagesHandlers += (ids -> handler)
    case CommandFailed(cmd: Tcp.Command) =>
      context.actorSelection("/user/statsSender") ! "Failed to execute command : " + cmd
    case nonsense: Any => logger.warn(s"NetworkController: got something strange $nonsense")
  }
}

object NetworkController {

  object ReceivableMessages {

    case class DataFromPeer(message: NetworkMessage, source: ConnectedPeer)

    case class RegisterMessagesHandler(types: Seq[(Byte, String)], handler: ActorRef)

    case class SendToNetwork(message: NetworkMessage, sendingStrategy: SendingStrategy)

    case class ConnectTo(address: InetSocketAddress)

  }

}