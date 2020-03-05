package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern._
import akka.util.Timeout
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.{Equal, Older, Younger}
import encry.network.ConnectedPeersCollection.PeerInfo
import encry.network.DM.{IsRequested, RequestSent}
import encry.network.MessageBuilder.{GetPeerInfo, GetPeerWithEqualHistory, GetPeerWithOlderHistory, GetPeers}
import encry.network.Messages.MessageToNetwork
import encry.network.Messages.MessageToNetwork.{BroadcastModifier, RequestFromLocal, ResponseFromLocal, SendSyncInfo}
import encry.network.PeerConnectionHandler.ConnectedPeer
import org.encryfoundation.common.network.BasicMessagesRepo.{InvNetworkMessage, ModifiersNetworkMessage, RequestModifiersNetworkMessage}
import org.encryfoundation.common.utils.Algos

import scala.concurrent.duration._
import scala.util.Try

case class MessageBuilder(msg: MessageToNetwork,
                          peersKeeper: ActorRef,
                          deliveryManager: ActorRef) extends Actor with StrictLogging {

  import context.dispatcher

  implicit val timeout: Timeout = Timeout(10 seconds)

  override def receive: Receive = {
    case RequestFromLocal(Some(peer), modTypeId, modsIds) =>
      Try {
        (peersKeeper ? GetPeerInfo(peer)).mapTo[ConnectedPeer].foreach { peer =>
          modsIds.foreach { modId =>
            for {
              isRequested <- (deliveryManager ? IsRequested(modId)).mapTo[Boolean]
            } yield if (!isRequested) {
              peer.handlerRef ! RequestModifiersNetworkMessage(modTypeId -> modsIds)
              deliveryManager ! RequestSent(peer.socketAddress, modTypeId, modId)
            } else logger.debug(s"Duplicate request for modifier of type ${modTypeId} and id: ${Algos.encode(modId)}")
          }
        }
      }
    case RequestFromLocal(None, modTypeId, modsIds) =>
      Try {
        (peersKeeper ? (GetPeerWithOlderHistory() || GetPeerWithEqualHistory())).mapTo[ConnectedPeer].foreach { peer =>
          modsIds.foreach { modId =>
            for {
              isRequested <- (deliveryManager ? IsRequested(modId)).mapTo[Boolean]
            } yield if (!isRequested) {
              peer.handlerRef ! RequestModifiersNetworkMessage(modTypeId -> modsIds)
              deliveryManager ! RequestSent(peer.socketAddress, modTypeId, modId)
            } else logger.debug(s"Duplicate request for modifier of type ${modTypeId} and id: ${Algos.encode(modId)}")
          }
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

  trait GetPeerByPredicate {
    def predicate: PeerInfo => Boolean
    def ||(that: GetPeerByPredicate): GetPeerByPredicate = new GetPeerByPredicate {
      override def predicate: PeerInfo => Boolean = info => this.predicate(info) || that.predicate(info)
    }
    def &&(that: GetPeerByPredicate): GetPeerByPredicate =new GetPeerByPredicate {
      override def predicate: PeerInfo => Boolean = info => this.predicate(info) && that.predicate(info)
    }
  }
  final case class GetPeerWithEqualHistory() extends GetPeerByPredicate {
    override def predicate: PeerInfo => Boolean = (info: PeerInfo) => info.historyComparisonResult == Equal
  }

  final case class GetPeerWithOlderHistory() extends GetPeerByPredicate {
    override def predicate: PeerInfo => Boolean = (info: PeerInfo) => info.historyComparisonResult == Older
  }

  final case class GetPeerWithYoungerHistory() extends GetPeerByPredicate {
    override def predicate: PeerInfo => Boolean = (info: PeerInfo) => info.historyComparisonResult == Younger
  }

  def props(msg: MessageToNetwork,
            peersKeeper: ActorRef,
            deliveryManager: ActorRef): Props = Props(new MessageBuilder(msg, peersKeeper, deliveryManager))
}