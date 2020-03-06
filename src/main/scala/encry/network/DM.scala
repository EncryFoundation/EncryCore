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
import encry.nvg.nvhg.NodeViewHolder.{SemanticallyFailedModification, SemanticallySuccessfulModifier}

import scala.collection.mutable
import scala.concurrent.Future

case class DM(networkSettings: NetworkSettings) extends Actor with StrictLogging {

  import context.dispatcher

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  var expectedModifiers: Set[ModifierIdAsKey] = Set.empty
  var receivedModifier: Set[ModifierIdAsKey] = Set.empty

  override def preStart(): Unit =
    context.parent ! RegisterMessagesHandler(Seq(ModifiersNetworkMessage.NetworkMessageTypeID -> "ModifiersNetworkMessage"), self)

  override def receive: Receive = {
    case DataFromPeer(ModifiersNetworkMessage(data), source) =>
      data._2.foreach { case (id, bytes) =>
        if (expectedModifiers.contains(toKey(id))) {
          context.parent ! ModifierFromNetwork(source, data._1, id, bytes)
          expectedModifiers -= toKey(id)
          receivedModifier += toKey(id)
        } else logger.info("Receive spam!")
      }
    case RequestSent(peer, modTypeId, modId) =>
      expectedModifiers += toKey(modId)
      context.system.scheduler.scheduleOnce(networkSettings.deliveryTimeout)(self !
        AwaitingRequest(peer, modTypeId, modId, 1)
      )
    case AwaitingRequest(peer, modTypeId, modId, attempts) if attempts <= networkSettings.maxDeliveryChecks && expectedModifiers.contains(toKey(modId))=>
      context.parent ! RequestFromLocal(peer.some, modTypeId, List(modId))
      context.system.scheduler.scheduleOnce(networkSettings.deliveryTimeout)(self !
        AwaitingRequest(peer, modTypeId, modId, attempts + 1)
      )
    case AwaitingRequest(peer, _, modId, _) =>
      logger.info(s"Stop requesting modifier ${Algos.encode(modId)} from peer $peer")
    case ModifierFromNetwork(source, modTypeId, modId, modBytes) =>
      if (expectedModifiers.contains(toKey(modId))) {
        expectedModifiers -= toKey(modId)
        receivedModifier += toKey(modId)
        context.parent ! ModifierFromNetwork(source, modTypeId, modId, modBytes)
      } else logger.info(s"Peer $source sent spam mod of type $modTypeId and id ${Algos.encode(modId)}")
    case SemanticallySuccessfulModifier(mod) => receivedModifier -= toKey(mod.id)
    case SemanticallyFailedModification(mod, _) => receivedModifier -= toKey(mod.id)
    case IsRequested(modIds) =>
      //logger.info(s"Going to check if ${Algos.encode(modId)} has been requested. Res: ${receivedModifier.contains(toKey(modId))}")
      sender ! RequestStatus(
        modIds.filter(id => receivedModifier.contains(toKey(id)) || expectedModifiers.contains(toKey(id))),
        modIds.filter(id => !receivedModifier.contains(toKey(id)) && !expectedModifiers.contains(toKey(id)))
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
