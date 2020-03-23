package encry.nvg

import java.io.File

import akka.actor.Actor
import cats.syntax.option._
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.nvg.IntermediaryNVHView.IntermediaryNVHViewActions.RegisterHistory
import encry.nvg.IntermediaryNVHView.ModifierToAppend
import encry.nvg.NVHHistory.{ ModifierAppliedToHistory, ProgressInfoForState }
import encry.nvg.NVHState.StateAction
import encry.nvg.NodeViewHolder.{
  SemanticallyFailedModification,
  SemanticallySuccessfulModifier,
  SyntacticallyFailedModification
}
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.{ ModifierAppendedToHistory, StartApplyingModifier }
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.history.History.HistoryUpdateInfoAcc
import encry.view.history.{ History, HistoryReader }
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ Header, Payload }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ ModifierId, ModifierTypeId }

class NVHHistory(settings: EncryAppSettings, ntp: NetworkTimeProvider) extends Actor with StrictLogging {

  var history: History = initializeHistory

  context.parent ! RegisterHistory(HistoryReader(history))

  override def receive: Receive = {
    case ModifierToAppend(mod, isLocallyGenerated) if !history.isModifierDefined(mod.id) =>
      val startProcessingTime: Long = System.currentTimeMillis()
      logger.info(s"Start modifier ${mod.encodedId} of type ${mod.modifierTypeId} processing by history.")
      context.parent ! StartApplyingModifier(mod.id, mod.modifierTypeId, startProcessingTime)
      history.append(mod) match {
        case Left(error: Throwable) =>
          logger.info(
            s"Error ${error.getMessage} has occurred during processing modifier by history component. " +
              s"Time of processing is: ${(System.currentTimeMillis() - startProcessingTime) / 1000}s."
          )
          context.parent ! SyntacticallyFailedModification(mod, List(HistoryApplyError(error.getMessage)))
          context.parent ! ModifierAppliedToHistory
        case Right((progressInfo: ProgressInfo, newUpdateInformation: Option[HistoryUpdateInfoAcc])) =>
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
            context.parent ! ProgressInfoForState(
              progressInfo,
              (history.getBestHeaderHeight - history.getBestBlockHeight - 1) < settings.constants.MaxRollbackDepth * 2,
              history.isFullChainSynced
            )
            history.insertUpdateInfo(newUpdateInformation)
            if (!isLocallyGenerated) progressInfo.toApply.foreach {
              case header: Header => requestDownloads(progressInfo, header.id.some)
              case _              => requestDownloads(progressInfo, none)
            }
          } else {
            logger.info(s"Progress info contains an empty toApply. Going to form request download.")
            if (!isLocallyGenerated) requestDownloads(progressInfo, mod.id.some)
            context.parent ! SemanticallySuccessfulModifier(mod)
          }
      }

    case ModifierToAppend(mod, _) =>
      context.parent ! ModifierAppliedToHistory
      logger.info(s"Got modifier ${mod.encodedId} on history actor which already contains in history.")

    case StateAction.ModifierApplied(mod: PersistentModifier) =>
      history = history.reportModifierIsValid(mod)
      context.parent ! HistoryReader(history)
      context.parent ! ModifierAppliedToHistory

    case StateAction.ApplyFailed(mod, e) =>
      val (newHistory: History, progressInfo: ProgressInfo) = history.reportModifierIsInvalid(mod)
      context.parent ! SemanticallyFailedModification(mod, e)
      context.parent ! ProgressInfoForState(
        progressInfo,
        (history.getBestHeaderHeight - history.getBestBlockHeight - 1) < settings.constants.MaxRollbackDepth * 2,
        history.isFullChainSynced
      )
      history = newHistory

  }

  def requestDownloads(pi: ProgressInfo, previousModifier: Option[ModifierId] = none): Unit =
    pi.toDownload.foreach {
      case (tid: ModifierTypeId, id: ModifierId) =>
        if (tid != Payload.modifierTypeId || (history.isFullChainSynced && tid == Payload.modifierTypeId)) {
          logger.info(
            s"History holder created download request for modifier ${Algos.encode(id)} of type $tid. " +
              s"Previous modifier is ${previousModifier.map(Algos.encode)}."
          )
          context.parent ! RequestFromLocal(none, tid, List(id))
        } else
          logger.info(
            s"Ignore sending download request for modifier ${Algos.encode(id)} because full chain is not synced."
          )
    }

  def initializeHistory: History =
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
        initializeHistory
    }
}

object NVHHistory {
  final case class ProgressInfoForState(pi: ProgressInfo,
                                        saveRootNodeFlag: Boolean,
                                        isFullChainSynced: Boolean,
                                        reader: HistoryReader)
  case object ModifierAppliedToHistory
  final case object InsertNewUpdates
}
