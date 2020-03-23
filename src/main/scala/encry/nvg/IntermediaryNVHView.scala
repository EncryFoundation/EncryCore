package encry.nvg

import java.io.File

import akka.actor.{Actor, ActorRef, Props}
import cats.syntax.option.none
import encry.network.NetworkRouter.ModifierFromNetwork
import encry.nvg.IntermediaryNVHView.IntermediaryNVHViewActions.{RegisterHistory, RegisterState}
import encry.nvg.NVHState.StateAction
import encry.nvg.NodeViewHolder.NodeView
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.history.{History, HistoryReader}
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import org.apache.commons.io.FileUtils

class IntermediaryNVHView(settings: EncryAppSettings,
                          ntp: NetworkTimeProvider,
                          influx: Option[ActorRef]) extends Actor {

  override def preStart(): Unit = {
    //init history?
  }

  override def receive: Receive = awaitingViewActors()

  def awaitingViewActors(history: Option[ActorRef] = None, state: Option[ActorRef] = None): Receive = {
    case RegisterHistory(reader) if state.isEmpty =>
      context.become(awaitingViewActors(Some(sender()), state), discardOld = true)
      context.actorOf(
        NVHState.restoreConsistentStateProps(settings, reader, influx).getOrElse (
          NVHState.genesisProps(settings, influx)
        )
      )
    case RegisterHistory(_) =>
      context.become(viewReceive(sender(), state.get))
    case RegisterState if history.isEmpty =>
      context.become(awaitingViewActors(history, Some(sender())), discardOld = true)
    case RegisterHistory =>
      context.become(viewReceive(history.get, sender()))
  }

  def viewReceive(history: ActorRef, state: ActorRef): Receive = {
    case ModifierFromNetwork(remote, typeId, modifierId, modifierBytes) => history ! ModifierFromNetwork
    case StateAction.ApplyFailed(modId, errs) =>
    // todo: Notify history

  }
}

object IntermediaryNVHView {

  sealed trait IntermediaryNVHViewActions
  object IntermediaryNVHViewActions {
    case class RegisterHistory(historyReader: HistoryReader) extends IntermediaryNVHViewActions
    case object RegisterState extends IntermediaryNVHViewActions
  }

  def props(settings: EncryAppSettings,
            ntp: NetworkTimeProvider,
            influxRef: Option[ActorRef]): Props = Props(new IntermediaryNVHView(settings, ntp, influxRef))
}
