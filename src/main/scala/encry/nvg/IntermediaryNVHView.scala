package encry.nvg

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.BlockAndHeaderInfo
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.nvg.IntermediaryNVHView.IntermediaryNVHViewActions.{RegisterHistory, RegisterState}
import encry.nvg.IntermediaryNVHView.{InitGenesisHistory, ModifierToAppend}
import encry.nvg.ModifiersValidator.ValidatedModifier
import encry.nvg.NVHHistory.{ModifierAppliedToHistory, ProgressInfoForState}
import encry.nvg.NVHState.StateAction
import encry.nvg.NVHState.StateAction.ApplyModifier
import encry.nvg.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import encry.nvg.NodeViewHolder.{SemanticallyFailedModification, SemanticallySuccessfulModifier, SyntacticallyFailedModification}
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.StatsSenderMessage
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.NetworkTimeProvider
import encry.view.history.HistoryReader
import encry.view.state.UtxoStateReader
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.IndexedSeq
import scala.util.{Failure, Success}

class IntermediaryNVHView(settings: EncryAppSettings, ntp: NetworkTimeProvider, influx: Option[ActorRef])
    extends Actor
    with StrictLogging {

  var historyReader: HistoryReader = HistoryReader.empty

  val historyRef: ActorRef = context.actorOf(NVHHistory.props(ntp, settings))

  var isModifierProcessingInProgress: Boolean = false

  var toApply = Set.empty[ByteArrayWrapper]

  override def receive: Receive = awaitingViewActors()

  def awaitingViewActors(history: Option[ActorRef] = None,
                         state: Option[ActorRef] = None,
                         stateReader: Option[UtxoStateReader] = None): Receive = {
    case RegisterHistory(reader) if state.isEmpty =>
      historyReader = reader
      logger.info(s"NodeViewParent actor got init history. Going to init state actor.")
      context.become(awaitingViewActors(Some(sender()), state), discardOld = true)
      context.actorOf(
        NVHState
          .restoreConsistentStateProps(settings, reader, influx)
          .getOrElse {
            historyRef ! InitGenesisHistory
            NVHState.genesisProps(settings, influx)
          }
      )
    case RegisterHistory(_) =>
      context.become(viewReceive(sender(), state.get, stateReader.get))
    case RegisterState(reader) if history.isEmpty =>
      context.become(awaitingViewActors(history, Some(sender()), Some(reader)), discardOld = true)
    case RegisterHistory =>
      context.become(viewReceive(history.get, sender(), stateReader.get))
  }

  def viewReceive(history: ActorRef, state: ActorRef, stateReader: UtxoStateReader): Receive = {

    case RegisterState(reader) => context.become(viewReceive(history, sender(), reader))

    case LocallyGeneratedModifier(modifier: PersistentModifier) =>
      ModifiersCache.put(
        NodeViewHolder.toKey(modifier.id),
        modifier,
        historyReader,
        settings,
        isLocallyGenerated = true
      )
      if (!isModifierProcessingInProgress) getNextModifier()

    case ValidatedModifier(modifier: PersistentModifier) =>
      val isInHistory: Boolean = historyReader.isModifierDefined(modifier.id)
      val isInCache: Boolean   = ModifiersCache.contains(NodeViewHolder.toKey(modifier.id))
      if (isInHistory || isInCache)
        logger.info(
          s"Modifier ${modifier.encodedId} can't be placed into the cache cause: " +
            s"contains in cache: $isInCache, contains in history: $isInHistory."
        )
      else
        ModifiersCache.put(
          NodeViewHolder.toKey(modifier.id),
          modifier,
          historyReader,
          settings,
          isLocallyGenerated = false
        )
      if (!isModifierProcessingInProgress) getNextModifier()
    case ModifierAppliedToHistory             => isModifierProcessingInProgress = false; getNextModifier()
    case msg: ProgressInfoForState if msg.pi.chainSwitchingNeeded && msg.pi.branchPoint.exists(point => !stateReader.version.sameElements(point))=>
      context.become(viewReceive(
        history,
        context.actorOf(
          NVHState.rollbackProps(
            settings,
            influx,
            stateReader.safePointHeight,
            msg.pi.branchPoint.get,
            historyReader,
            settings.constants
          )
        ),
        stateReader
      ))
    case msg: ProgressInfoForState =>
      toApply = msg.pi.toApply.map(mod => ByteArrayWrapper(mod.id)).toSet
      msg.pi.toApply.foreach(mod => state ! StateAction.ApplyModifier(mod, msg.saveRootNodeFlag, msg.isFullChainSynced))
    case msg: StateAction.ApplyFailed         => historyRef ! msg
    case msg: StateAction.ModifierApplied     =>
      historyRef ! msg
    case msg: SyntacticallyFailedModification => context.parent ! msg
    case msg: StatsSenderMessage              => context.parent ! msg
    case msg: RequestFromLocal                => context.parent ! msg
    case msg: SemanticallyFailedModification  => context.parent ! msg
    case msg: SemanticallySuccessfulModifier  => context.parent ! msg
    case msg: BlockAndHeaderInfo              => context.parent ! msg
    case msg: HistoryReader                   => historyReader = msg; context.parent ! msg
  }

  def awaitingHistoryBranchPoint(history: ActorRef): Receive = ???

  def getNextModifier(): Unit =
    ModifiersCache
      .popCandidate(historyReader, settings)
      .foreach {
        case (mod: PersistentModifier, isLocallyGenerated) =>
          isModifierProcessingInProgress = true
          logger.info(s"Got new modifiers in compute application function: ${mod.encodedId}.")
          historyRef ! ModifierToAppend(mod, isLocallyGenerated)
      }

}

object IntermediaryNVHView {

  sealed trait IntermediaryNVHViewActions
  object IntermediaryNVHViewActions {
    case class RegisterHistory(historyReader: HistoryReader) extends IntermediaryNVHViewActions
    case class RegisterState(stateReader: UtxoStateReader) extends IntermediaryNVHViewActions
  }

  final case class ModifierToAppend(modifier: PersistentModifier, isLocallyGenerated: Boolean)
  case object InitGenesisHistory

  def props(settings: EncryAppSettings, ntp: NetworkTimeProvider, influxRef: Option[ActorRef]): Props =
    Props(new IntermediaryNVHView(settings, ntp, influxRef))
}
