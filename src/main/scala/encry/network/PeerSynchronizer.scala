package encry.network

import java.net.InetSocketAddress

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import encry.EncryApp.{networkController, peerManager, settings}
import scorex.core.network.message.{GetPeersSpec, Message, MessageSpec, PeersSpec}
import scorex.core.network.{SendToPeers, SendToRandom}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.ScorexLogging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps


class PeerSynchronizer extends Actor with ScorexLogging {

  import encry.network.NetworkController.ReceivableMessages.{RegisterMessagesHandler, SendToNetwork}
  import encry.network.peer.PeerManager.ReceivableMessages.{AddOrUpdatePeer, RandomPeers}
  import encry.network.NetworkController.ReceivableMessages.DataFromPeer

  val networkSettings: NetworkSettings = settings.network

  implicit val timeout: Timeout = Timeout(networkSettings.syncTimeout.getOrElse(5 seconds))

  val messageSpecs: Seq[MessageSpec[_]] = Seq(GetPeersSpec, PeersSpec)

  override def preStart: Unit = {
    super.preStart()
    networkController ! RegisterMessagesHandler(messageSpecs, self)
    val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
    val stn = SendToNetwork(msg, SendToRandom)
    context.system.scheduler.schedule(2.seconds, 10.seconds)(networkController ! stn)
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
          networkController ! SendToNetwork(Message(PeersSpec, Right(peers), None), SendToPeers(Seq(remote)))
        }
    case nonsense: Any => log.warn(s"PeerSynchronizer: got something strange $nonsense")
  }
}