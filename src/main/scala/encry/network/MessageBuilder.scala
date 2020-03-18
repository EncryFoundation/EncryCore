package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern._
import akka.util.Timeout
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.{Equal, Older, Younger}
import encry.network.ConnectedPeersCollection.PeerInfo
import encry.network.DM.{IsRequested, RequestSent, RequestStatus}
import encry.network.MessageBuilder.{GetPeerInfo, GetPeers, MsgSent}
import encry.network.Messages.MessageToNetwork.{BroadcastModifier, RequestFromLocal, ResponseFromLocal, SendPeers, SendSyncInfo}
import encry.network.PeerConnectionHandler.ConnectedPeer
import org.encryfoundation.common.network.BasicMessagesRepo.{InvNetworkMessage, ModifiersNetworkMessage, PeersNetworkMessage, RequestModifiersNetworkMessage, SyncInfoNetworkMessage}
import org.encryfoundation.common.utils.Algos

import scala.concurrent.duration._
import scala.util.Try

case class MessageBuilder(peersKeeper: ActorRef,
                          deliveryManager: ActorRef) extends Actor with StrictLogging {

  import context.dispatcher

  implicit val timeout: Timeout = Timeout(10 seconds)

  override def receive: Receive = {
    case RequestFromLocal(Some(peer), modTypeId, modsIds) =>
      Try {
        (peersKeeper ? GetPeerInfo(peer)).mapTo[ConnectedPeer].map { peer =>
          logger.info(s"Going to req mods from ${peer.socketAddress} of type ${modTypeId}")
          (deliveryManager ? IsRequested(modsIds)).mapTo[RequestStatus].foreach { status =>
            peer.handlerRef ! RequestModifiersNetworkMessage(modTypeId -> status.notRequested)
            status.notRequested.foreach(modId => deliveryManager ! RequestSent(peer.socketAddress, modTypeId, modId))
            logger.info(s"Send req for mods: ${status.notRequested.map(Algos.encode).mkString(",")} to peer ${peer.socketAddress}")
            logger.info(s"Requested or received: ${status.requested.length}. Not request or not received: ${status.notRequested.length}")
            context.parent ! MsgSent(RequestModifiersNetworkMessage.NetworkMessageTypeID, peer.socketAddress)
          }
        }
      }
    case RequestFromLocal(None, modTypeId, modsIds) =>
      Try {
        (peersKeeper ? (MessageBuilder.PeerWithOlderHistory || MessageBuilder.PeerWithEqualHistory)).mapTo[ConnectedPeer].map { peer =>
          logger.info(s"Going to req mods from ${peer.socketAddress} of type ${modTypeId}")
          (deliveryManager ? IsRequested(modsIds)).mapTo[RequestStatus].foreach { status =>
            logger.info(s"Requested or received: ${status.requested.length}. Not request or not received: ${status.notRequested.length}")
            peer.handlerRef ! RequestModifiersNetworkMessage(modTypeId -> status.notRequested)
            modsIds.foreach(modId => deliveryManager ! RequestSent(peer.socketAddress, modTypeId, modId))
            context.parent ! MsgSent(RequestModifiersNetworkMessage.NetworkMessageTypeID, peer.socketAddress)
          }
        }
      }
    case SendSyncInfo(syncInfo) =>
      (peersKeeper ? GetPeers).mapTo[List[ConnectedPeer]].map { peers =>
        peers.foreach(_.handlerRef ! SyncInfoNetworkMessage(syncInfo))
        context.parent ! MsgSent(SyncInfoNetworkMessage.NetworkMessageTypeID, peers.head.socketAddress)
      }
    case ResponseFromLocal(peer, modTypeId, modsIds) =>
      Try {
        (peersKeeper ? GetPeerInfo(peer)).mapTo[ConnectedPeer].map { peer =>
          peer.handlerRef ! ModifiersNetworkMessage(modTypeId -> modsIds)
          context.parent ! MsgSent(ModifiersNetworkMessage.NetworkMessageTypeID, peer.socketAddress)
        }
      }
      context.stop(self)
    case BroadcastModifier(modTypeId, modInfo) =>
      (peersKeeper ? GetPeers).mapTo[List[ConnectedPeer]].map { peers =>
        peers.foreach(_.handlerRef ! InvNetworkMessage(modTypeId -> List(modInfo)))
        context.parent ! MsgSent(InvNetworkMessage.NetworkMessageTypeID, peers.head.socketAddress)
      }
      context.stop(self)
    case SendPeers(peers, remote) =>
      Try {
        (peersKeeper ? GetPeerInfo(remote)).mapTo[ConnectedPeer].map { peer =>
          peer.handlerRef ! PeersNetworkMessage(peers)
          context.parent ! MsgSent(PeersNetworkMessage.NetworkMessageTypeID, peer.socketAddress)
        }
      }
  }
}

object MessageBuilder {

  case object GetPeers
  case class MsgSent(msgType: Byte, receiver: InetSocketAddress)
  case class GetPeerInfo(peerIp: InetSocketAddress)

  trait GetPeerByPredicate {

    def predicate: PeerInfo => Boolean

    def ||(that: GetPeerByPredicate): GetPeerByPredicate = {
      GetPeerByPredicate((info: PeerInfo) => predicate(info) || that.predicate(info))
    }
    def &&(that: GetPeerByPredicate): GetPeerByPredicate = {
      val newPredicate = (info: PeerInfo) => this.predicate.andThen(res => that.predicate(info) && res)(info)
      GetPeerByPredicate(newPredicate)
    }
  }

  object GetPeerByPredicate {
    def apply(peerPredicate: PeerInfo => Boolean): GetPeerByPredicate = new GetPeerByPredicate {
      override def predicate: PeerInfo => Boolean = peerPredicate
    }
  }

  val PeerWithEqualHistory = GetPeerByPredicate((info: PeerInfo) => info.historyComparisonResult == Equal)
  val PeerWithOlderHistory = GetPeerByPredicate((info: PeerInfo) => info.historyComparisonResult == Older)
  val PeerWithYoungerHistory = GetPeerByPredicate((info: PeerInfo) => info.historyComparisonResult == Younger)

  def props(peersKeeper: ActorRef,
            deliveryManager: ActorRef): Props = Props(new MessageBuilder(peersKeeper, deliveryManager))
}