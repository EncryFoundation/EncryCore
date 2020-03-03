package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.network.DM.{AwaitingRequest, RequestSent}
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.NetworkRouter.ModifierFromNetwork
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.NetworkSettings
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

import scala.collection.mutable

case class DM(networkSettings: NetworkSettings) extends Actor with StrictLogging {

  import context.dispatcher

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  var expectedModifiers: Set[ModifierIdAsKey] = Set.empty
  var receivedModifier: Set[ModifierIdAsKey] = Set.empty

  override def receive: Receive = {
    case RequestSent(peer, modTypeId, modId) =>
      expectedModifiers += toKey(modId)
      context.system.scheduler.scheduleOnce(networkSettings.deliveryTimeout)(self !
        AwaitingRequest(peer, modTypeId, modId, 1)
      )
    case AwaitingRequest(peer, modTypeId, modId, attempts) if attempts <= networkSettings.maxDeliveryChecks =>
      if (expectedModifiers.contains(toKey(modId))) context.parent ! RequestFromLocal(peer, modTypeId, List(modId))
    case AwaitingRequest(peer, _, modId, _) =>
      logger.info(s"Stop requesting modifier ${Algos.encode(modId)} from peer $peer")
    case ModifierFromNetwork(source, modTypeId, modId, modBytes) =>
      if (expectedModifiers.contains(toKey(modId))) {
        expectedModifiers -= toKey(modId)
        receivedModifier += toKey(modId)
        context.parent ! ModifierFromNetwork(source, modTypeId, modId, modBytes)
      } else logger.info(s"Peer $source sent spam mod of type $modTypeId and id ${Algos.encode(modId)}")
    case SemanticallySuccessfulModifier(mod) => receivedModifier -= toKey(mod.id)
  }

  def toKey(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)
}

object DM {

  case class AwaitingRequest(peer: InetSocketAddress, modTypeId: ModifierTypeId, modId: ModifierId, attempts: Int)
  case class RequestSent(peer: InetSocketAddress, modTypeId: ModifierTypeId, modId: ModifierId)


  def props(): Props = ???
}
