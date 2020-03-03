package encry.view

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.settings.EncryAppSettings
import encry.view.NodeViewHolder.ReceivableMessages.{CompareViews, GetDataFromCurrentView}
import encry.view.NodeViewHolder._
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
