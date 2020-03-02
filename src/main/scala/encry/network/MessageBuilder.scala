package encry.network

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern._
import akka.util.Timeout
import encry.network.MessageBuilder.GetPeers
import encry.network.Messages.MessageToNetwork
import encry.network.Messages.MessageToNetwork.{RequestFromLocal, SendSyncInfo}

import scala.concurrent.duration._

case class MessageBuilder(msg: MessageToNetwork, peersKeeper: ActorRef) extends Actor {

  import context.dispatcher

  implicit val timeout: Timeout = Timeout(10 seconds)

  override def receive: Receive = {
    case RequestFromLocal(peer, modTypeId, modsIds) =>
    case SendSyncInfo(syncInfo) =>
      (peersKeeper ? GetPeers).mapTo[List[ActorRef]].map { peers =>
        peers.foreach(_ ! syncInfo)
      }
  }
}

object MessageBuilder {

  case object GetPeers

  def props(msg: MessageToNetwork, peersKeeper: ActorRef): Props = Props(new MessageBuilder(msg, peersKeeper))
}