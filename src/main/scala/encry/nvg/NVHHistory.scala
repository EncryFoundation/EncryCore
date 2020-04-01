package encry.nvg

import java.io.File

import akka.actor.{ Actor, Props }
import cats.syntax.option._
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.api.http.DataHolderForApi.BlockAndHeaderInfo
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.local.miner.Miner.EnableMining
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.nvg.IntermediaryNVHView.IntermediaryNVHViewActions.RegisterNodeView
import encry.nvg.IntermediaryNVHView.{ InitGenesisHistory, ModifierToAppend }
import encry.nvg.NVHHistory.{ ModifierAppliedToHistory, NewWalletReader, ProgressInfoForState }
import encry.nvg.NVHState.StateAction
import encry.nvg.NodeViewHolder.{
  SemanticallyFailedModification,
  SemanticallySuccessfulModifier,
  SyntacticallyFailedModification
}
import encry.settings.EncryAppSettings
import encry.stats.StatsSender._
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.history.History.HistoryUpdateInfoAcc
import encry.view.history.{ History, HistoryReader }
import encry.view.wallet.{ EncryWallet, WalletReader }
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ Block, Header, Payload }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ ModifierId, ModifierTypeId }

class NVHHistory(settings: EncryAppSettings, ntp: NetworkTimeProvider)
    extends Actor
    with StrictLogging
    with AutoCloseable {

  final case class HistoryView(history: History, wallet: EncryWallet)

  var historyView: HistoryView = initializeHistory.getOrElse(genesis)

  var lastProgressInfo: ProgressInfo = ProgressInfo(none, Seq.empty, Seq.empty, none)

  context.parent ! RegisterNodeView(HistoryReader(historyView.history), WalletReader(historyView.wallet))

  var modsInToApply: List[String] = List.empty[String]

  override def postStop(): Unit = println("stop!")

  override def receive: Receive = {
    case ModifierToAppend(mod, isLocallyGenerated) if !historyView.history.isModifierDefined(mod.id) =>
      val startProcessingTime: Long = System.currentTimeMillis()
      logger.info(s"Start modifier ${mod.encodedId} of type ${mod.modifierTypeId} processing by history.")
      context.parent ! StartApplyingModifier(mod.id, mod.modifierTypeId, startProcessingTime)
      historyView.history.append(mod) match {
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
          historyView.history.insertUpdateInfo(newUpdateInformation)
          if (mod.modifierTypeId == Header.modifierTypeId) historyView.history.updateIdsForSyncInfo()
          context.parent ! EndOfApplyingModifier(mod.id)
          context.parent ! ModifierAppendedToHistory(mod match {
            case _: Header  => true
            case _: Payload => false
          }, success = true)
          if (progressInfo.toApply.nonEmpty) {
            logger.info(s"Progress info contains an non empty toApply. Going to notify state about new toApply.")
            modsInToApply = progressInfo.toApply.map(_.encodedId).toList
            context.parent ! ProgressInfoForState(
              progressInfo,
              (historyView.history.getBestHeaderHeight - historyView.history.getBestBlockHeight - 1) < settings.constants.MaxRollbackDepth * 2,
              historyView.history.isFullChainSynced,
              HistoryReader(historyView.history)
            )
            lastProgressInfo = progressInfo
            if (!isLocallyGenerated) progressInfo.toApply.foreach {
              case header: Header => requestDownloads(progressInfo, header.id.some)
              case _              => requestDownloads(progressInfo, none)
            }
          } else {
            logger.info(s"Progress info contains an empty toApply. Going to form request download.")
            context.parent ! ModifierAppliedToHistory
            if (!isLocallyGenerated) requestDownloads(progressInfo, mod.id.some)
            context.parent ! HeightStatistics(historyView.history.getBestHeaderHeight, -1) //todo incorrect state height
            context.parent ! SemanticallySuccessfulModifier(mod)
          }
      }

    case ModifierToAppend(mod, _) =>
      context.parent ! ModifierAppliedToHistory
      logger.info(s"Got modifier ${mod.encodedId} on history actor which already contains in history.")

    case StateAction.ModifierApplied(mod: PersistentModifier) =>
      val newHistory = historyView.history.reportModifierIsValid(mod)
      historyView = historyView.copy(history = newHistory)
      historyView.history.getBestHeader.foreach(context.parent ! BestHeaderInChain(_))
      context.parent ! HistoryReader(historyView.history)
      context.parent ! BlockAndHeaderInfo(historyView.history.getBestHeader, historyView.history.getBestBlock)
      if (historyView.history.getBestHeaderId.exists(
            besId => historyView.history.getBestBlockId.exists(_.sameElements(besId))
          )) {
        logger.info(s"\n\n\nGot message StateAction.ModifierApplied with mod ${mod.encodedId} of type ${mod.modifierTypeId}. " +
          s"Set up historyView.history.isFullChainSynced = true. Condition is: " +
          s" (historyView.history.getBestHeaderId: ${(historyView.history.getBestHeaderId.map(Algos.encode))}. " +
          s" (historyView.history.getBestBlockId.exists(_.sameElements(besId)): ${historyView.history.getBestBlockId.map(Algos.encode)}.\n\n\n")
        historyView.history.isFullChainSynced = true
      }
      context.parent ! HeightStatistics(historyView.history.getBestHeaderHeight, -1) //todo incorrect state height
      if (mod match {
            case _: Block   => true
            case _: Payload => true
            case _          => false
          }) context.parent ! ModifierAppendedToState(success = true)
      if (lastProgressInfo.chainSwitchingNeeded)
        historyView.wallet.rollback(VersionTag !@@ lastProgressInfo.branchPoint.get).get
      historyView.wallet.scanPersistent(mod)
      context.parent ! NewWalletReader(WalletReader(historyView.wallet))
      context.parent ! SemanticallySuccessfulModifier(mod)
      if (historyView.history.isFullChainSynced) context.system.eventStream.publish(FullBlockChainIsSynced)
      if (settings.node.mining && historyView.history.isFullChainSynced)
        context.system.eventStream.publish(EnableMining)
      modsInToApply = modsInToApply.filterNot(_ == mod.encodedId)
      if (modsInToApply.isEmpty) context.parent ! ModifierAppliedToHistory

    case StateAction.ApplyFailed(mod, e) =>
      val (newHistory: History, progressInfo: ProgressInfo) = historyView.history.reportModifierIsInvalid(mod)
      historyView = historyView.copy(history = newHistory)
      context.parent ! SemanticallyFailedModification(mod, e)
      modsInToApply = progressInfo.toApply.map(_.encodedId).toList
      context.parent ! ProgressInfoForState(
        progressInfo,
        (historyView.history.getBestHeaderHeight - historyView.history.getBestBlockHeight - 1) < settings.constants.MaxRollbackDepth * 2,
        historyView.history.isFullChainSynced,
        HistoryReader(historyView.history)
      )
      lastProgressInfo = progressInfo

    case InitGenesisHistory =>
      logger.info("Init in InitGenesisHistory")
      historyView.history.close()
      historyView.wallet.close()
      historyView = genesis

  }

  def requestDownloads(pi: ProgressInfo, previousModifier: Option[ModifierId] = none): Unit =
    pi.toDownload.foreach {
      case (tid: ModifierTypeId, id: ModifierId) =>
        if (tid != Payload.modifierTypeId || (historyView.history.isFullChainSynced && tid == Payload.modifierTypeId)) {
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

  def initializeHistory: Option[HistoryView] =
    try {
      val history: History = History.readOrGenerate(settings, ntp)
      history.updateIdsForSyncInfo()
      val wallet: EncryWallet =
        EncryWallet.readOrGenerate(EncryWallet.getWalletDir(settings), EncryWallet.getKeysDir(settings), settings)
      logger.info(s"History best block height: ${history.getBestBlockHeight}")
      logger.info(s"History best header height: ${history.getBestHeaderHeight}")
      Some(HistoryView(history, wallet))
    } catch {
      case error: Throwable =>
        logger.info(s"During history initialization error ${error.getMessage} has happened.")
        None
    }

  def genesis: HistoryView =
    try {
      new File(s"${settings.directory}/history/").listFiles.foreach(FileUtils.cleanDirectory)
      new File(s"${settings.directory}/wallet/").listFiles.foreach(FileUtils.cleanDirectory)
      new File(s"${settings.directory}/keys/").listFiles.foreach(FileUtils.cleanDirectory)
      val history: History = History.readOrGenerate(settings, ntp)
      history.updateIdsForSyncInfo()
      val wallet: EncryWallet =
        EncryWallet.readOrGenerate(EncryWallet.getWalletDir(settings), EncryWallet.getKeysDir(settings), settings)
      logger.info(s"History best block height: ${history.getBestBlockHeight}")
      logger.info(s"History best header height: ${history.getBestHeaderHeight}")
      HistoryView(history, wallet)
    } catch {
      case error: Throwable =>
        EncryApp.forceStopApplication(1,
                                      s"During genesis history initialization error ${error.getMessage} has happened.")
    }

  override def close(): Unit = {
    historyView.history.close()
    historyView.wallet.close()
  }
}

object NVHHistory {
  final case class ProgressInfoForState(pi: ProgressInfo,
                                        saveRootNodeFlag: Boolean,
                                        isFullChainSynced: Boolean,
                                        reader: HistoryReader)
  case object ModifierAppliedToHistory
  final case object InsertNewUpdates
  final case class NewWalletReader(reader: WalletReader)
  def props(ntp: NetworkTimeProvider, settings: EncryAppSettings): Props = Props(new NVHHistory(settings, ntp))
}
