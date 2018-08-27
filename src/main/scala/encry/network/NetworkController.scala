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
import encry.network.peer.PeerManager._
import encry.network.message.{Message, MessageHandler}
import encry.network.peer.PeerManager.ReceivableMessages.{CheckPeers, Disconnected, FilterPeers}
import encry.settings.NetworkSettings
import encry.stats.LoggingActor.LogMessage
import encry.view.history.EncrySyncInfoMessageSpec
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}

class NetworkController extends Actor {

  val networkSettings: NetworkSettings = settings.network
  context.actorOf(Props[PeerSynchronizer].withDispatcher("network-dispatcher"), "peerSynchronizer")
  val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ Seq(EncrySyncInfoMessageSpec))
  var messageHandlers: Map[Seq[MessageCode], ActorRef] = Map.empty
  var outgoing: Set[InetSocketAddress] = Set.empty
  lazy val externalSocketAddress: Option[InetSocketAddress] = networkSettings.declaredAddress orElse None

  if (!networkSettings.localOnly) {
    networkSettings.declaredAddress.foreach { myAddress =>
      Try {
        val myAddrs: Array[InetAddress] = InetAddress.getAllByName(new URI("http://" + myAddress).getHost)
        NetworkInterface.getNetworkInterfaces.asScala.exists { intf =>
          intf.getInterfaceAddresses.asScala.exists { intfAddr => myAddrs.contains(intfAddr.getAddress) }
        }
      } recover { case t: Throwable =>
        if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
          LogMessage("Error", s"Declared address validation failed: $t", System.currentTimeMillis())
      }
    }
  }

  if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
    LogMessage("Info", s"Declared address: $externalSocketAddress", System.currentTimeMillis())

  IO(Tcp) ! Bind(self, networkSettings.bindAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  override def supervisorStrategy: SupervisorStrategy = commonSupervisorStrategy

  def bindingLogic: Receive = {
    case Bound(_) =>
      if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
        LogMessage("Info", "Successfully bound to the port " + networkSettings.bindAddress.getPort, System.currentTimeMillis())
      context.system.scheduler.schedule(600.millis, 5.seconds)(peerManager ! CheckPeers)
    case CommandFailed(_: Bind) =>
      if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
        LogMessage("Info", "Network port " + networkSettings.bindAddress.getPort + " already in use!", System.currentTimeMillis())
      context stop self
  }

  def businessLogic: Receive = {
    case Message(spec, Left(msgBytes), Some(remote)) =>
      spec.parseBytes(msgBytes) match {
        case Success(content) =>
          messageHandlers.find(_._1.contains(spec.messageCode)).map(_._2) match {
            case Some(handler) => handler ! DataFromPeer(spec, content, remote)
            case None => if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
              LogMessage("Error", "No handlers found for message: " + spec.messageCode, System.currentTimeMillis())
          }
        case Failure(e) => if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
          LogMessage("Error", s"Failed to deserialize data: $e", System.currentTimeMillis())
      }
    case SendToNetwork(message, sendingStrategy) =>
      (peerManager ? FilterPeers(sendingStrategy)) (5 seconds)
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! message))
  }

  def peerLogic: Receive = {
    case ConnectTo(remote)
      if checkPossibilityToAddPeer(remote) =>
      if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
        LogMessage("Info", s"Connecting to: $remote", System.currentTimeMillis())
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
      if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
        LogMessage("Info", logMsg, System.currentTimeMillis())
      context.actorOf(PeerConnectionHandler.props(messagesHandler, sender(), direction, externalSocketAddress, remote)
        .withDispatcher("network-dispatcher"))
      outgoing -= remote
    case CommandFailed(c: Connect) =>
      outgoing -= c.remoteAddress
      if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
        LogMessage("Info", "Failed to connect to : " + c.remoteAddress, System.currentTimeMillis())
      peerManager ! Disconnected(c.remoteAddress)
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse {
    case RegisterMessagesHandler(specs, handler) =>
      if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
        LogMessage("Info", s"Registering handlers for ${specs.map(s => s.messageCode -> s.messageName)}", System.currentTimeMillis())
      messageHandlers += specs.map(_.messageCode) -> handler
    case CommandFailed(cmd: Tcp.Command) => context.actorSelection("/user/statsSender") ! "Failed to execute command : " + cmd
    case nonsense: Any => if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
      LogMessage("Warn", s"NetworkController: got something strange $nonsense", System.currentTimeMillis())
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
