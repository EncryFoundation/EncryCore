package encry.nvg

import java.io.File

import akka.actor.Actor
import cats.syntax.option._
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.nvg.IntermediaryNVHView.IntermediaryNVHViewActions.RegisterHistory
import encry.nvg.IntermediaryNVHView.ModifierToAppend
import encry.nvg.NVHHistory.ProgressInfoForState
import encry.nvg.NodeViewHolder.{SemanticallySuccessfulModifier, SyntacticallyFailedModification}
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.{EndOfApplyingModifier, ModifierAppendedToHistory, StartApplyingModifier}
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.history.History.AwaitingAppendToHistory
import encry.view.history.{History, HistoryReader}
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

class NVHHistory(settings: EncryAppSettings, ntp: NetworkTimeProvider) extends Actor with StrictLogging {

  var history: History = initHistory

  context.parent ! RegisterHistory(HistoryReader(history))

  var historyUpdateInformation: Option[AwaitingAppendToHistory] = None

  override def receive: Receive = {
    case ModifierToAppend(mod, isLocallyGenerated) if !history.isModifierDefined(mod.id) =>
      val startProcessingTime: Long = System.currentTimeMillis()
      logger.info(s"Start modifier ${mod.encodedId} of type ${mod.modifierTypeId} processing by history.")
      context.parent ! StartApplyingModifier(mod.id, mod.modifierTypeId, startProcessingTime)
      history.append(mod) match {
        case Left(error) =>
          logger.info(s"Error ${error.getMessage} has occurred during processing modifier by history component.")
          context.parent ! SyntacticallyFailedModification(mod, List(HistoryApplyError(error.getMessage)))
        case Right((progressInfo, newUpdateInformation)) =>
          logger.info(
            s"Modifier ${mod.encodedId} of type ${mod.modifierTypeId} processed successfully by history. " +
              s"Time of processing is: ${(System.currentTimeMillis() - startProcessingTime) / 1000}s."
          )
          context.parent ! ModifierAppendedToHistory(mod match {
            case _: Header  => true
            case _: Payload => false
          }, success = true)
          if (progressInfo.toApply.nonEmpty) {
            logger.info(s"Progress info contains an non empty toApply. Going to notify state about new toApply.")
            historyUpdateInformation = newUpdateInformation
            context.parent ! ProgressInfoForState(progressInfo)
          } else {
            logger.info(s"Progress info contains an empty toApply. Going to form request download.")
            if (!isLocallyGenerated) requestDownloads(progressInfo, mod.id.some)
            context.parent ! SemanticallySuccessfulModifier(mod)
          }
      }

    case ModifierToAppend(mod, _)  =>
      logger.info(s"Got modifier ${mod.encodedId} on history actor which already contains in history.")
  }

  def requestDownloads(pi: ProgressInfo, previousModifier: Option[ModifierId] = none): Unit =
    pi.toDownload.foreach {
      case (tid: ModifierTypeId, id: ModifierId) =>
        logger.info(
          s"Node view holder created download request for modifier ${Algos.encode(id)} of type $tid. " +
            s"Previous modifier is ${previousModifier.map(Algos.encode)}."
        )
        if (tid != Payload.modifierTypeId || (history.isFullChainSynced && tid == Payload.modifierTypeId))
          context.parent ! RequestFromLocal(none, tid, List(id))
        else
          logger.info(
            s"Ignore sending download request for modifier ${Algos.encode(id)} because full chain is not synced."
          )
    }

  def initHistory: History =
    try {
      val history: History = History.readOrGenerate(settings, ntp)
      history.updateIdsForSyncInfo()
      logger.info(s"History best block height: ${history.getBestBlockHeight}")
      logger.info(s"History best header height: ${history.getBestHeaderHeight}")
      history
    } catch {
      case error: Throwable =>
        logger.info(s"During history initialization error ${error.getMessage} has happened.")
        new File(settings.directory).listFiles.foreach(FileUtils.cleanDirectory)
        initHistory
    }
}

object NVHHistory {
  final case class ProgressInfoForState(pi: ProgressInfo) extends AnyVal
}
