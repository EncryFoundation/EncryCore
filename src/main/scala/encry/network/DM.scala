package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.pattern._
import com.typesafe.scalalogging.StrictLogging
import encry.network.DM.{AwaitingRequest, IsRequested, RequestSent, RequestStatus}
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.NetworkRouter.ModifierFromNetwork
import encry.settings.NetworkSettings
import org.encryfoundation.common.network.BasicMessagesRepo.ModifiersNetworkMessage
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import cats.syntax.option._
import encry.nvg.NodeViewHolder.{SemanticallyFailedModification, SemanticallySuccessfulModifier}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction

import scala.collection.mutable
import scala.concurrent.Future

case class DM(networkSettings: NetworkSettings) extends Actor with StrictLogging {

  import context.dispatcher

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  var expectedModifiers: Set[ModifierIdAsKey] = Set.empty
  var receivedModifiers: Set[ModifierIdAsKey] = Set.empty

  override def preStart(): Unit =
    context.parent ! RegisterMessagesHandler(Seq(ModifiersNetworkMessage.NetworkMessageTypeID -> "ModifiersNetworkMessage"), self)

  override def receive: Receive = {
    case DataFromPeer(ModifiersNetworkMessage(data), source) =>
      data._2.foreach { case (id, bytes) =>
        if (expectedModifiers.contains(toKey(id))) {
          context.parent ! ModifierFromNetwork(source, data._1, id, bytes)
          expectedModifiers -= toKey(id)
          receivedModifiers += toKey(id)
        } else logger.info(s"Receive spam. ModId: ${Algos.encode(id)}!")
      }
    case RequestSent(peer, modTypeId, modId) if !(expectedModifiers.contains(toKey(modId)) || receivedModifiers.contains(toKey(modId)))=>
      expectedModifiers += toKey(modId)
      context.system.scheduler.scheduleOnce(networkSettings.deliveryTimeout)(
        self ! AwaitingRequest(peer, modTypeId, modId, 1)
      )
    case RequestSent(_, _, _) => //do nothing
    case AwaitingRequest(peer, modTypeId, modId, attempts) if
      attempts <= networkSettings.maxDeliveryChecks && expectedModifiers.contains(toKey(modId)) && modTypeId != Transaction.modifierTypeId =>
        context.parent ! RequestFromLocal(peer.some, modTypeId, List(modId))
        logger.info(s"Re-request modifier ${Algos.encode(modId)}")
        context.system.scheduler.scheduleOnce(networkSettings.deliveryTimeout)(self !
          AwaitingRequest(peer, modTypeId, modId, attempts + 1)
        )
    case AwaitingRequest(peer, _, modId, attempts) =>
      logger.info(s"Stop requesting modifier ${Algos.encode(modId)} from peer $peer, qty of attempts $attempts." +
        s" Expected modifier contains: ${expectedModifiers.contains(toKey(modId))}")
      expectedModifiers -= toKey(modId)
    case ModifierFromNetwork(source, modTypeId, modId, modBytes) =>
      if (expectedModifiers.contains(toKey(modId))) {
        expectedModifiers -= toKey(modId)
        receivedModifiers += toKey(modId)
        context.parent ! ModifierFromNetwork(source, modTypeId, modId, modBytes)
      } else logger.info(s"Peer $source sent spam mod of type $modTypeId and id ${Algos.encode(modId)}")
    case SemanticallySuccessfulModifier(mod) => receivedModifiers -= toKey(mod.id)
    case SemanticallyFailedModification(mod, _) => receivedModifiers -= toKey(mod.id)
    case IsRequested(modIds) =>
      //logger.info(s"Going to check if ${Algos.encode(modId)} has been requested. Res: ${receivedModifier.contains(toKey(modId))}")
      sender ! RequestStatus(
        requested = modIds.filter(id => receivedModifiers.contains(toKey(id)) || expectedModifiers.contains(toKey(id))),
        notRequested = modIds.filter(id => !receivedModifiers.contains(toKey(id)) && !expectedModifiers.contains(toKey(id)))
      )
  }

  def toKey(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)
}

object DM {

  case class AwaitingRequest(peer: InetSocketAddress, modTypeId: ModifierTypeId, modId: ModifierId, attempts: Int)
  case class RequestSent(peer: InetSocketAddress, modTypeId: ModifierTypeId, modId: ModifierId)
  case class IsRequested(modifiersId: List[ModifierId])
  case class RequestStatus(requested: List[ModifierId], notRequested: List[ModifierId])
  def props(networkSettings: NetworkSettings): Props = Props(new DM(networkSettings))
}
