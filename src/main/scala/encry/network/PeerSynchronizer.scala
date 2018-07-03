package encry.network

import java.net.InetSocketAddress

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import encry.EncryApp._
import encry.network.message.{GetPeersSpec, Message, PeersSpec}
import shapeless.syntax.typeable._

import scala.concurrent.duration._
import scala.language.postfixOps
import encry.network.NetworkController.ReceivableMessages.{RegisterMessagesHandler, SendToNetwork}
import encry.network.peer.PeerManager.ReceivableMessages.{AddOrUpdatePeer, RandomPeers}
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.utils.ScorexLogging

class PeerSynchronizer extends Actor with ScorexLogging {

  implicit val timeout: Timeout = Timeout(settings.network.syncTimeout.getOrElse(5 seconds))

  override def preStart: Unit = {
    super.preStart()
    networkController ! RegisterMessagesHandler(Seq(GetPeersSpec, PeersSpec), self)
    val msg: Message[Unit] = Message[Unit](GetPeersSpec, Right(Unit), None)
    context.system.scheduler.schedule(2.seconds, 10.seconds)(networkController ! SendToNetwork(msg, SendToRandom))
  }

  override def receive: Receive = {
    case DataFromPeer(spec, peers: Seq[InetSocketAddress]@unchecked, remote)
      if spec.messageCode == PeersSpec.messageCode && peers.cast[Seq[InetSocketAddress]].isDefined =>
      peers.foreach(isa => peerManager ! AddOrUpdatePeer(isa, None, Some(remote.direction)))
    case DataFromPeer(spec, _, remote) if spec.messageCode == GetPeersSpec.messageCode =>
      //todo: externalize the number, check on receiving
      (peerManager ? RandomPeers(3))
        .mapTo[Seq[InetSocketAddress]]
        .foreach { peers =>
          networkController ! SendToNetwork(Message(PeersSpec, Right(peers), None), SendToPeer(remote))
        }
    case nonsense: Any => log.warn(s"PeerSynchronizer: got something strange $nonsense")
  }
}