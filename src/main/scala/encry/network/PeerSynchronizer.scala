package encry.network

import java.net.InetSocketAddress
import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import encry.EncryApp._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.message.{GetPeersSpec, Message, PeersSpec}
import encry.network.peer.PeerManager._
import encry.network.peer.PeerManager.ReceivableMessages.{AddOrUpdatePeer, RandomPeers}
import encry.stats.LoggingActor.LogMessage
import scala.concurrent.duration._
import scala.language.postfixOps

class PeerSynchronizer extends Actor {

  implicit val timeout: Timeout = Timeout(settings.network.syncTimeout.getOrElse(5 seconds))

  override def preStart: Unit = {
    super.preStart()
    networkController ! RegisterMessagesHandler(Seq(GetPeersSpec, PeersSpec), self)
    val msg: Message[Unit] = Message[Unit](GetPeersSpec, Right(Unit), None)
    context.system.scheduler.schedule(2.seconds, settings.network.syncInterval)(networkController ! SendToNetwork(msg, SendToRandom))
  }

  override def receive: Receive = {
    case DataFromPeer(spec, peers: Seq[InetSocketAddress]@unchecked, remote)
      if spec.messageCode == PeersSpec.messageCode =>
      peers.filter(checkPossibilityToAddPeer).foreach(isa =>
        peerManager ! AddOrUpdatePeer(isa, None, Some(remote.direction)))
      if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
        LogMessage("Debug", s"Get new peers: [${peers.mkString(",")}] from ${remote.socketAddress}", System.currentTimeMillis())
    case DataFromPeer(spec, _, remote) if spec.messageCode == GetPeersSpec.messageCode =>
      (peerManager ? RandomPeers(3))
        .mapTo[Seq[InetSocketAddress]]
        .foreach { peers =>
          val correctPeers: Seq[InetSocketAddress] = peers.filter(address => {
            if (remote.socketAddress.getAddress.isSiteLocalAddress) true
            else !address.getAddress.isSiteLocalAddress && address != remote.socketAddress
          })
          if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
            LogMessage("Info", s"Remote is side local: ${remote.socketAddress} : ${remote.socketAddress.getAddress.isSiteLocalAddress}", System.currentTimeMillis())
          networkController ! SendToNetwork(Message(PeersSpec, Right(correctPeers), None), SendToPeer(remote))
          if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
            LogMessage("Debug", s"Send to ${remote.socketAddress} peers message which contains next peers: ${peers.mkString(",")}", System.currentTimeMillis())
        }
    case nonsense: Any => if (settings.logging.enableLogging) context.system.actorSelection("/user/loggingActor") !
      LogMessage("Warn", s"PeerSynchronizer: got something strange $nonsense", System.currentTimeMillis())
  }
}