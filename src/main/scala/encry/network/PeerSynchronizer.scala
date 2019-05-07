package encry.network

import scala.concurrent.duration._
import scala.language.postfixOps
import encry.network.NetworkController.ReceivableMessages._
import java.net.InetSocketAddress
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import encry.settings.EncryAppSettings
import akka.util.Timeout
import com.typesafe.scalalogging.StrictLogging
import encry.cli.commands.AddPeer.PeerFromCli
import PeerManager.ReceivableMessages.{AddOrUpdatePeer, RandomPeers}
import BasicMessagesRepo._

class PeerSynchronizer(networkController: ActorRef,
                       settings: EncryAppSettings,
                       peerManager: ActorRef) extends Actor with StrictLogging {

  import context.dispatcher

  implicit val timeout: Timeout = Timeout(settings.network.syncTimeout.getOrElse(5 seconds))
  var knownPeersCollection: Set[InetSocketAddress] = settings.network.knownPeers.toSet

  override def preStart: Unit = {
    super.preStart()
    networkController ! RegisterMessagesHandler(Seq(
      PeersNetworkMessage.NetworkMessageTypeID -> "PeersNetworkMessage",
      GetPeersNetworkMessage.NetworkMessageTypeID -> "GetPeersNetworkMessage"
    ), self)
    context.system.scheduler.schedule(2.seconds,
      settings.network.syncInterval)(networkController ! SendToNetwork(GetPeersNetworkMessage, SendToRandom))
  }

  override def receive: Receive = {
    case DataFromPeer(message, remote) => message match {
      case PeersNetworkMessage(peers) =>
        peers.filter(p => CheckPeersObj.checkPossibilityToAddPeer(p, knownPeersCollection, settings)).foreach(isa =>
          peerManager ! AddOrUpdatePeer(isa, None, Some(remote.direction)))
        logger.debug(s"Got new peers: [${peers.mkString(",")}] from ${remote.socketAddress}")
      case GetPeersNetworkMessage =>
        (peerManager ? RandomPeers(3)).mapTo[Seq[InetSocketAddress]]
          .foreach { peers =>
            val correctPeers: Seq[InetSocketAddress] = peers.filter(address => {
              if (remote.socketAddress.getAddress.isSiteLocalAddress) true
              else !address.getAddress.isSiteLocalAddress && address != remote.socketAddress
            })
            logger.info(s"Remote is side local: ${remote.socketAddress} : ${remote.socketAddress.getAddress.isSiteLocalAddress}")
            remote.handlerRef ! PeersNetworkMessage(correctPeers)
            logger.debug(s"Send to ${remote.socketAddress} peers message which contains next peers: ${peers.mkString(",")}")
          }
      case _ => logger.info(s"PeerSynchronizer got invalid type of DataFromPeer message!")
    }
    case PeerFromCli(address) =>
      knownPeersCollection = knownPeersCollection + address
    case nonsense: Any => logger.warn(s"PeerSynchronizer: got something strange $nonsense")
  }
}

object PeerSynchronizer {

  def props(networkController: ActorRef, settings: EncryAppSettings, peerManager: ActorRef): Props =
    Props(new PeerSynchronizer(networkController, settings, peerManager))
}