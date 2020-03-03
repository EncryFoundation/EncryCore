package encry.view

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.settings.EncryAppSettings
import encry.view.NodeViewHolder.ReceivableMessages.{CompareViews, GetDataFromCurrentView}
import encry.view.NodeViewHolder._
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.Seq
import scala.concurrent.Future

class NodeViewHolder(memoryPoolRef: ActorRef,
                     influxRef: Option[ActorRef],
                     dataHolder: ActorRef,
                     encrySettings: EncryAppSettings)
    extends Actor
    with StrictLogging
    with AutoCloseable {

  override def receive: Receive = {
    case GetDataFromCurrentView(f) =>
      f(CurrentView(nodeView.history, nodeView.state, nodeView.wallet)) match {
        case resultFuture: Future[_] => resultFuture.pipeTo(sender())
        case result                  => sender() ! result
      }

    case GetNodeViewChanges(history, state, _) =>
      if (history) sender() ! ChangedHistory(nodeView.history)
      if (state) sender() ! ChangedState(nodeView.state)

    case CompareViews(peer, modifierTypeId, modifierIds) =>
      logger.info(s"Start processing CompareViews message on NVH.")
      val startTime = System.currentTimeMillis()
      val ids: Seq[ModifierId] = modifierTypeId match {
        case _ =>
          modifierIds
            .filterNot(mid => nodeView.history.isModifierDefined(mid) || ModifiersCache.contains(key(mid)))
      }
      if (modifierTypeId != Transaction.modifierTypeId)
        logger.debug(
          s"Got compare view message on NVH from ${peer.socketAddress}." +
            s" Type of requesting modifiers is: $modifierTypeId. Requesting ids size are: ${ids.size}." +
            s" Sending RequestFromLocal with ids to $sender." +
            s"\n Requesting ids are: ${ids.map(Algos.encode).mkString(",")}."
        )
      if (ids.nonEmpty && (modifierTypeId == Header.modifierTypeId || (nodeView.history.isHeadersChainSynced && modifierTypeId == Payload.modifierTypeId)))
        sender() ! RequestFromLocal(peer, modifierTypeId, ids)
      logger.debug(
        s"Time processing of msg CompareViews from $sender with modTypeId $modifierTypeId: ${System.currentTimeMillis() - startTime}"
      )
    case SemanticallySuccessfulModifier(_) =>
    case msg                               => logger.error(s"Got strange message on nvh: $msg")
  }

}

object NodeViewHolder {

  case class CurrentView[HIS, MS, VL](history: HIS, state: MS, vault: VL)


  object ReceivableMessages {

    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean)

    case class GetDataFromCurrentView[HIS, MS, VL, A](f: CurrentView[HIS, MS, VL] => A)

    case object GetWallet

    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  }

  class NodeViewHolderPriorityQueue(settings: ActorSystem.Settings, config: Config)
      extends UnboundedStablePriorityMailbox(PriorityGenerator {
        case CompareViews(_, _, _) => 0

        case PoisonPill => 2

        case otherwise => 1
      })

  def props(memoryPoolRef: ActorRef,
            influxRef: Option[ActorRef],
            dataHolder: ActorRef,
            settings: EncryAppSettings): Props =
    Props(new NodeViewHolder(memoryPoolRef, influxRef, dataHolder, settings))
}
