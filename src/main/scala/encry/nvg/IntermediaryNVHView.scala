package encry.nvg

import akka.actor.{ Actor, ActorRef, Props }
import com.typesafe.scalalogging.StrictLogging
import encry.nvg.IntermediaryNVHView.IntermediaryNVHViewActions.{ RegisterHistory, RegisterState }
import encry.nvg.IntermediaryNVHView.ModifierToAppend
import encry.nvg.ModifiersValidator.ValidatedModifier
import encry.nvg.NVHHistory.{ ModifierAppliedToHistory, ProgressInfoForState }
import encry.nvg.NVHState.StateAction
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.history.HistoryReader
import org.encryfoundation.common.modifiers.PersistentModifier

import scala.collection.mutable

class IntermediaryNVHView(settings: EncryAppSettings, ntp: NetworkTimeProvider, influx: Option[ActorRef])
    extends Actor
    with StrictLogging {

  var historyReader: HistoryReader = HistoryReader.empty

  val historyRef: ActorRef = ActorRef.noSender

  var isProcessingModifierByHistory: Boolean = false

  var isProcessingModifierByState: Boolean = false

  override def receive: Receive = awaitingViewActors()

  def awaitingViewActors(history: Option[ActorRef] = None, state: Option[ActorRef] = None): Receive = {
    case RegisterHistory(reader) if state.isEmpty =>
      historyReader = reader
      logger.info(s"NodeViewParent actor got init history. Going to init state actor.")
      context.become(awaitingViewActors(Some(sender()), state), discardOld = true)
      context.actorOf(
        NVHState
          .restoreConsistentStateProps(settings, reader, influx)
          .getOrElse(
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
    case ValidatedModifier(modifier: PersistentModifier) =>
      val wrappedKey: mutable.WrappedArray.ofByte = NodeViewHolder.toKey(modifier.id)

      val isInHistory: Boolean = historyReader.isModifierDefined(modifier.id)

      val isInCache: Boolean = ModifiersCache.contains(wrappedKey)

      if (isInHistory || isInCache)
        logger.info(
          s"Modifier ${modifier.encodedId} can't be placed into the cache cause: " +
            s"contains in cache: $isInCache, contains in history: $isInHistory."
        )
      else ModifiersCache.put(wrappedKey, modifier, historyReader, settings)

      if (!isProcessingModifierByHistory && !isProcessingModifierByState)
        getNextModifier()

    case ModifierAppliedToHistory =>
      isProcessingModifierByHistory = false
      getNextModifier()

    case ProgressInfoForState(pi, flag, isFullChainSynced) => //todo work with state starts here
    case msg: StateAction.ApplyFailed                      => historyRef ! msg
    case msg: StateAction.ModifierApplied                  => historyRef ! msg
  }

  def awaitingHistoryBranchPoint(history: ActorRef): Receive = ???

  def getNextModifier(): Unit =
    ModifiersCache
      .popCandidate(historyReader, settings)
      .foreach { mod: PersistentModifier =>
        isProcessingModifierByHistory = true
        logger.info(s"Got new modifiers in compute application function: ${mod.encodedId}.")
        historyRef ! ModifierToAppend(mod, isLocallyGenerated = false)
      }

}

object IntermediaryNVHView {

  sealed trait IntermediaryNVHViewActions
  object IntermediaryNVHViewActions {
    case class RegisterHistory(historyReader: HistoryReader) extends IntermediaryNVHViewActions
    case object RegisterState                                extends IntermediaryNVHViewActions
  }

  final case class ModifierToAppend(modifier: PersistentModifier, isLocallyGenerated: Boolean)

  def props(settings: EncryAppSettings, ntp: NetworkTimeProvider, influxRef: Option[ActorRef]): Props =
    Props(new IntermediaryNVHView(settings, ntp, influxRef))
}
