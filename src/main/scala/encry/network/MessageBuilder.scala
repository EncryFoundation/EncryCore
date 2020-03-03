package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern._
import akka.util.Timeout
import encry.network.MessageBuilder.{GetPeerInfo, GetPeers}
import encry.network.Messages.MessageToNetwork
import encry.network.Messages.MessageToNetwork.{BroadcastModifier, RequestFromLocal, ResponseFromLocal, SendSyncInfo}
import encry.network.PeerConnectionHandler.ConnectedPeer
import org.encryfoundation.common.network.BasicMessagesRepo.{InvNetworkMessage, ModifiersNetworkMessage, RequestModifiersNetworkMessage}

import scala.concurrent.duration._
import scala.util.Try

case class MessageBuilder(msg: MessageToNetwork, peersKeeper: ActorRef) extends Actor {

  import context.dispatcher

  implicit val timeout: Timeout = Timeout(10 seconds)

  override def receive: Receive = {
    case RequestFromLocal(peer, modTypeId, modsIds) =>
      Try {
        (peersKeeper ? GetPeerInfo(peer)).mapTo[ConnectedPeer].map { peer =>
          peer.handlerRef ! RequestModifiersNetworkMessage(modTypeId -> modsIds)
        }
      }
    case SendSyncInfo(syncInfo) =>
      (peersKeeper ? GetPeers).mapTo[List[ConnectedPeer]].map { peers =>
        peers.foreach(_.handlerRef ! syncInfo)
      }
    case ResponseFromLocal(peer, modTypeId, modsIds) =>
      Try {
        (peersKeeper ? GetPeerInfo(peer)).mapTo[ConnectedPeer].map { peer =>
          peer.handlerRef ! ModifiersNetworkMessage(modTypeId -> modsIds)
        }
      }
    case BroadcastModifier(modTypeId, modInfo) =>
      (peersKeeper ? GetPeers).mapTo[List[ConnectedPeer]].map { peers =>
        peers.foreach(_.handlerRef ! InvNetworkMessage(modTypeId -> List(modInfo)))
      }
  }
}

object MessageBuilder {

  case object GetPeers
  case class GetPeerInfo(peerIp: InetSocketAddress)

  def props(msg: MessageToNetwork, peersKeeper: ActorRef): Props = Props(new MessageBuilder(msg, peersKeeper))
}