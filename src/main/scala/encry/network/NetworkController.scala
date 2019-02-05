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
import encry.network.message.Message.MessageCode
import PeerManager._
import encry.network.message.{Message, MessageHandler}
import PeerManager.ReceivableMessages.{CheckPeers, Disconnected, FilterPeers}
import com.typesafe.scalalogging.StrictLogging
import encry.settings.NetworkSettings
import encry.view.history.EncrySyncInfoMessageSpec
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}

class NetworkController extends Actor with StrictLogging {

  val networkSettings: NetworkSettings = settings.network
  context.actorOf(Props[PeerSynchronizer].withDispatcher("network-dispatcher"), "peerSynchronizer")
  val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ Seq(EncrySyncInfoMessageSpec))
  var messageHandlers: Map[Seq[MessageCode], ActorRef] = Map.empty
  var outgoing: Set[InetSocketAddress] = Set.empty
  lazy val externalSocketAddress: Option[InetSocketAddress] = networkSettings.declaredAddress orElse None

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

  IO(Tcp) ! Bind(self, networkSettings.bindAddress, options = KeepAlive(true) :: Nil)

  override def supervisorStrategy: SupervisorStrategy = commonSupervisorStrategy

  def bindingLogic: Receive = {
    case Bound(localAddress) =>
      logger.info(s"Successfully bound to the address: $localAddress.")
      context.system.scheduler.schedule(600.millis, 5.seconds)(peerManager ! CheckPeers)
    case CommandFailed(_: Bind) =>
      logger.info("Network port " + networkSettings.bindAddress.getPort + " already in use!")
      context stop self
  }

  def businessLogic: Receive = {
    case Message(spec, Left(msgBytes), Some(remote)) =>
      spec.parseBytes(msgBytes) match {
        case Success(content) =>
          messageHandlers.find(_._1.contains(spec.messageCode)).map(_._2) match {
            case Some(handler) => handler ! DataFromPeer(spec, content, remote)
            case None => logger.error("No handlers found for message: " + spec.messageCode)
          }
        case Failure(e) => logger.error(s"Failed to deserialize data: $e")
      }
    case SendToNetwork(message, sendingStrategy) =>
      (peerManager ? FilterPeers(sendingStrategy)) (5 seconds)
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! message))
  }

  def peerLogic: Receive = {
    case ConnectTo(remote)
      if checkPossibilityToAddPeer(remote) =>
      logger.info(s"Connecting to: $remote")
      outgoing += remote
      IO(Tcp) ! Connect(remote,
        localAddress = externalSocketAddress,
        options = KeepAlive(true) :: Nil,
        timeout = Some(networkSettings.connectionTimeout),
        pullMode = true)
    case Connected(remote, local)
      if checkPossibilityToAddPeer(remote) =>
      val direction: ConnectionType = if (outgoing.contains(remote)) Outgoing else Incoming
      val logMsg: String = direction match {
        case Incoming => s"New incoming connection from $remote established (bound to local $local)"
        case Outgoing => s"New outgoing connection to $remote established (bound to local $local)"
      }
      logger.info(logMsg)
      context.actorOf(PeerConnectionHandler.props(messagesHandler, sender(), direction, externalSocketAddress, remote)
        .withDispatcher("network-dispatcher"))
      outgoing -= remote
    case Connected(remote, _) =>
      logger.info(s"Peer $remote trying to connect, but checkPossibilityToAddPeer(remote):" +
        s" ${checkPossibilityToAddPeer(remote)}.")
    case CommandFailed(c: Connect) =>
      outgoing -= c.remoteAddress
      logger.info("Failed to connect to : " + c.remoteAddress)
      peerManager ! Disconnected(c.remoteAddress)
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse {
    case RegisterMessagesHandler(specs, handler) =>
      logger.info(s"Registering handlers for ${specs.map(s => s.messageCode -> s.messageName)}")
      messageHandlers += specs.map(_.messageCode) -> handler
    case CommandFailed(cmd: Tcp.Command) =>
      context.actorSelection("/user/statsSender") ! "Failed to execute command : " + cmd
    case nonsense: Any => logger.warn(s"NetworkController: got something strange $nonsense")
  }
}

object NetworkController {

  object ReceivableMessages {

    import encry.network.message.MessageSpec
    import scala.reflect.runtime.universe.TypeTag

    case class DataFromPeer[DT: TypeTag](spec: MessageSpec[DT], data: DT, source: ConnectedPeer)

    case class RegisterMessagesHandler(specs: Seq[MessageSpec[_]], handler: ActorRef)

    case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)

    case class ConnectTo(address: InetSocketAddress)

  }

}