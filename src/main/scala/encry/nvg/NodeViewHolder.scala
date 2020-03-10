package encry.nvg

import java.io.File

import akka.actor.{ Actor, ActorRef, Props }
import akka.pattern._
import cats.syntax.option._
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.api.http.DataHolderForApi.BlockAndHeaderInfo
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.local.miner.Miner.{ DisableMining, StartMining }
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.nvg.ModifiersValidator.ValidatedModifier
import encry.nvg.NodeViewHolder.ReceivableMessages.{ CreateAccountManagerFromSeed, LocallyGeneratedModifier }
import encry.nvg.NodeViewHolder._
import encry.nvg.fast.sync.SnapshotProcessor.SnapshotManifest.ManifestId
import encry.nvg.fast.sync.SnapshotProcessor._
import encry.settings.EncryAppSettings
import encry.stats.StatsSender._
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewErrors.ModifierApplyError
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.NodeViewHolder.CurrentView
import encry.view.history.storage.HistoryStorage
import encry.view.history.{ History, HistoryHeadersProcessor, HistoryPayloadsProcessor, HistoryReader }
import encry.view.mempool.MemoryPool.RolledBackTransactions
import encry.view.state.UtxoState
import encry.view.state.avlTree.AvlTree
import encry.view.wallet.EncryWallet
import io.iohk.iodb.ByteArrayWrapper
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.modifiers.history.{ Block, Header, Payload }
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.{ PersistentModifier, PersistentNodeViewModifier }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ ADDigest, ModifierId, ModifierTypeId }

import scala.collection.{ mutable, IndexedSeq, Seq }
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{ Failure, Success, Try }

class NodeViewHolder(
  settings: EncryAppSettings,
  ntp: NetworkTimeProvider,
  influxRef: Option[ActorRef]
) extends Actor
    with StrictLogging
    with AutoCloseable {

  import context.dispatcher

  var nodeView: NodeView = restoreState().getOrElse(genesisState)

  var potentialManifestIds: List[ManifestId] = List.empty[ManifestId]

  context.parent ! UpdateHistoryReader(HistoryReader(nodeView.history))
  context.parent ! BlockAndHeaderInfo(nodeView.history.getBestHeader, nodeView.history.getBestBlock)

  context.system.scheduler.schedule(1.seconds, 10.seconds) {
    logger.info(
      s"\n\n History best header id is: ${nodeView.history.getBestHeaderId.map(Algos.encode)}.\n " +
        s"History best header height is: ${nodeView.history.getBestHeaderHeight}.\n " +
        s"History best block id is: ${nodeView.history.getBestBlockId.map(Algos.encode)}.\n " +
        s"History best block height is: ${nodeView.history.getBestBlockHeight}.\n " +
        s"History best block header is: ${nodeView.history.getHeaderOfBestBlock.map(_.encodedId)}.\n " +
        s"State height is: ${nodeView.state.height}.\n " +
        s"Cache size is: ${ModifiersCache.size}.\n "
    )
    logger.info(
      s"Cache elements are: ${ModifiersCache.cache.keys.toList.map(key => Algos.encode(key.toArray)).mkString(",")}."
    )
  }

  override def receive: Receive = {
    case ValidatedModifier(modifier: PersistentModifier) =>
      val startTime: Long                         = System.currentTimeMillis()
      val wrappedKey: mutable.WrappedArray.ofByte = NodeViewHolder.toKey(modifier.id)
      val isInHistory: Boolean                    = nodeView.history.isModifierDefined(modifier.id)
      val isInCache: Boolean                      = ModifiersCache.contains(wrappedKey)
      if (isInHistory || isInCache)
        logger.info(
          s"Modifier ${modifier.encodedId} can't be placed into the cache cause: " +
            s"contains in cache: $isInCache, contains in history: $isInHistory."
        )
      else ModifiersCache.put(wrappedKey, modifier, nodeView.history)
      computeApplications()
      logger.debug(
        s"Time of processing validated modifier with id: ${modifier.encodedId} " +
          s"is: ${(System.currentTimeMillis() - startTime) / 1000}s."
      )

    case LocallyGeneratedModifier(modifier: PersistentModifier) =>
      val startTime: Long = System.currentTimeMillis()
      logger.info(s"Got locally generated modifier ${modifier.encodedId}.")
      modifier match {
        case block: Block =>
          applyModifier(block.header, isLocallyGenerated = true)
          applyModifier(block.payload, isLocallyGenerated = true)
      }
      logger.debug(
        s"Time of processing locally generated modifier with id: ${modifier.encodedId} " +
          s"is ${(System.currentTimeMillis() - startTime) / 1000}s."
      )

    case GetDataFromCurrentView(f) =>
      logger.info("Receive GetDataFromCurrentView on nvh")
      f(CurrentView(nodeView.history, nodeView.state, nodeView.wallet)) match {
        case resultFuture: Future[_] => resultFuture.pipeTo(sender)
        case result                  => sender ! result
      }

    case FastSyncFinished(state: UtxoState, wallet: EncryWallet) =>
      val startTime: Long = System.currentTimeMillis()
      logger.info(s"Got a signal about finishing fast sync process.")
      nodeView.state.tree.avlStorage.close()
      nodeView.wallet.close()
      FileUtils.deleteDirectory(new File(s"${settings.directory}/tmpDirState"))
      FileUtils.deleteDirectory(new File(s"${settings.directory}/keysTmp"))
      FileUtils.deleteDirectory(new File(s"${settings.directory}/walletTmp"))
      val newHistory = new History with HistoryHeadersProcessor with HistoryPayloadsProcessor {
        override val settings: EncryAppSettings        = settings
        override var isFullChainSynced: Boolean        = settings.node.offlineGeneration
        override val timeProvider: NetworkTimeProvider = ntp
        override val historyStorage: HistoryStorage    = nodeView.history.historyStorage
      }
      newHistory.fastSyncInProgress.fastSyncVal = false
      newHistory.blockDownloadProcessor.updateMinimalBlockHeightVar(
        nodeView.history.blockDownloadProcessor.minimalBlockHeight
      )
      newHistory.isHeadersChainSyncedVar = true
      updateNodeView(
        updatedHistory = newHistory.some,
        updatedState = state.some,
        updatedVault = wallet.some
      )
      context.parent ! FastSyncDone
      logger.debug(s"Time of processing FastSyncDone message is: ${(System.currentTimeMillis() - startTime) / 1000}s.")

    case RemoveRedundantManifestIds => potentialManifestIds = List.empty

    case CreateAccountManagerFromSeed(seed) =>
      val newAccount: Either[String, EncryWallet] =
        nodeView.wallet.addAccount(seed, settings.wallet.map(_.password).get, nodeView.state)
      updateNodeView(updatedVault = newAccount.toOption)
      sender() ! newAccount

  }

  //todo refactor loop
  def computeApplications(): Unit = {
    val modifiers: List[PersistentModifier] = ModifiersCache.popCandidate(nodeView.history)
    if (modifiers.nonEmpty) {
      logger.info(s"Got new modifiers in compute application function: ${modifiers.map(_.encodedId)}.")
      modifiers.foreach(applyModifier(_))
      computeApplications()
    } else ()
  }

  def updateNodeView(
    updatedHistory: Option[History] = none,
    updatedState: Option[UtxoState] = none,
    updatedVault: Option[EncryWallet] = none
  ): Unit = {
    val newNodeView: NodeView = NodeView(
      updatedHistory.getOrElse(nodeView.history),
      updatedState.getOrElse(nodeView.state),
      updatedVault.getOrElse(nodeView.wallet)
    )
    if (updatedHistory.nonEmpty) context.parent ! UpdateHistoryReader(HistoryReader(newNodeView.history))
    nodeView = newNodeView
  }

  def requestDownloads(pi: ProgressInfo, previousModifier: Option[ModifierId] = none): Unit =
    pi.toDownload.foreach {
      case (tid: ModifierTypeId, id: ModifierId) =>
        logger.info(
          s"Node view holder created download request for modifier ${Algos.encode(id)} of type $tid. " +
            s"Previous modifier is ${previousModifier.map(Algos.encode)}."
        )
        if (tid != Payload.modifierTypeId || (nodeView.history.isFullChainSynced && tid == Payload.modifierTypeId))
          context.parent ! RequestFromLocal(none, tid, List(id))
        else
          logger.info(
            s"Ignore sending download request for modifier ${Algos.encode(id)} because full chain is not synced."
          )
    }

  def trimChainSuffix(
    suffix: IndexedSeq[PersistentModifier],
    rollbackPoint: ModifierId
  ): IndexedSeq[PersistentModifier] = {
    val idx: Int = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq.empty else suffix.drop(idx)
  }

  @scala.annotation.tailrec
  private def updateState(
    history: History,
    state: UtxoState,
    progressInfo: ProgressInfo,
    suffixApplied: IndexedSeq[PersistentModifier],
    isLocallyGenerated: Boolean = false
  ): (History, UtxoState, Seq[PersistentModifier]) = {
    logger.info(s"Starting updating state in updateState function!")
    if (!isLocallyGenerated) progressInfo.toApply.foreach {
      case header: Header => requestDownloads(progressInfo, header.id.some)
      case _              => requestDownloads(progressInfo, none)
    }
    val branchingPointOpt: Option[VersionTag] = progressInfo.branchPoint.map(VersionTag !@@ _)
    val (stateToApplyTry: Try[UtxoState], suffixTrimmed: IndexedSeq[PersistentModifier] @unchecked) =
      if (progressInfo.chainSwitchingNeeded) {
        branchingPointOpt.map { branchPoint: VersionTag =>
          if (!state.version.sameElements(branchPoint)) {
            val branchPointHeight: Int = history.getHeaderById(ModifierId !@@ branchPoint).get.height
            val additionalBlocks: List[Block] =
              (state.safePointHeight + 1 to branchPointHeight).foldLeft(List.empty[Block]) {
                case (blocks: List[Block], height: Int) =>
                  val headerAtHeight: Header = history.getBestHeaderAtHeight(height).get
                  val blockAtHeight: Block   = history.getBlockByHeader(headerAtHeight).get
                  blocks :+ blockAtHeight
              }
            context.parent ! DisableMining
            state.rollbackTo(branchPoint, additionalBlocks) -> trimChainSuffix(suffixApplied,
                                                                               ModifierId !@@ branchPoint)
          } else Success(state) -> IndexedSeq.empty
        }.getOrElse(Failure(new Exception("Trying to rollback when branchPoint is empty.")))
      } else Success(state) -> suffixApplied
    stateToApplyTry match {
      case Success(stateToApply: UtxoState) =>
        context.parent ! RollbackSucceed(branchingPointOpt)
        val u0: UpdateInformation = UpdateInformation(history, stateToApply, none, none, suffixTrimmed)
        val uf: UpdateInformation = progressInfo.toApply.foldLeft(u0) {
          case (u: UpdateInformation, modToApply: PersistentModifier) =>
            val saveRootNodesFlag: Boolean =
              (history.getBestHeaderHeight - history.getBestBlockHeight - 1) < settings.constants.MaxRollbackDepth * 2
            if (u.failedMod.isEmpty) u.state.applyModifier(modToApply, saveRootNodesFlag) match {
              case Right(stateAfterApply) =>
                modToApply match {
                  case b: Block if history.isFullChainSynced => context.parent ! TransactionsInBlock(b.payload.txs.size)
                  case _                                     =>
                }
                val newHis: History = history.reportModifierIsValid(modToApply)
                context.parent ! BlockAndHeaderInfo(newHis.getBestHeader, newHis.getBestBlock)
                modToApply match {
                  case header: Header =>
                    val requiredHeight: Int = header.height - settings.constants.MaxRollbackDepth
                    if (requiredHeight % settings.constants.SnapshotCreationHeight == 0) {
                      newHis.lastAvailableManifestHeight = requiredHeight
                      logger.info(s"heightOfLastAvailablePayloadForRequest -> ${newHis.lastAvailableManifestHeight}")
                    }
                  case _ =>
                }
                newHis.getHeaderOfBestBlock.foreach { header: Header =>
                  val potentialManifestId: Array[Byte] = Algos.hash(stateAfterApply.tree.rootHash ++ header.id)
                  val isManifestExists: Boolean        = potentialManifestIds.exists(_.sameElements(potentialManifestId))
                  val isCorrectCreationHeight: Boolean =
                    header.height % settings.constants.SnapshotCreationHeight == 0
                  val isGenesisHeader: Boolean = header.height == settings.constants.GenesisHeight
                  if (settings.snapshotSettings.enableSnapshotCreation && newHis.isFullChainSynced &&
                      !isManifestExists && isCorrectCreationHeight && !isGenesisHeader) {
                    val startTime = System.currentTimeMillis()
                    logger.info(s"Start chunks creation for new snapshot")
                    import encry.view.state.avlTree.utils.implicits.Instances._
                    val chunks: List[SnapshotChunk] =
                      AvlTree.getChunks(
                        stateAfterApply.tree.rootNode,
                        currentChunkHeight = settings.snapshotSettings.chunkDepth,
                        stateAfterApply.tree.avlStorage
                      )
                    context.parent ! TreeChunks(chunks, potentialManifestId)
                    potentialManifestIds = ManifestId @@ potentialManifestId :: potentialManifestIds
                    logger.info(
                      s"State tree successfully processed for snapshot. " +
                        s"Processing time is: ${(System.currentTimeMillis() - startTime) / 1000}s."
                    )
                  }
                }
                if (settings.node.mining && progressInfo.chainSwitchingNeeded)
                  context.parent ! StartMining
                context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
                if (newHis.getBestHeaderId.exists(
                      bestHeaderId =>
                        newHis.getBestBlockId.exists(bId => ByteArrayWrapper(bId) == ByteArrayWrapper(bestHeaderId))
                    )) newHis.isFullChainSynced = true
                context.parent ! HeightStatistics(newHis.getBestHeaderHeight, stateAfterApply.height)
                if (modToApply match {
                      case _: Block   => true
                      case _: Payload => true
                      case _          => false
                    }) context.parent ! ModifierAppendedToState(success = true)
                UpdateInformation(newHis, stateAfterApply, none, none, u.suffix :+ modToApply)
              case Left(e: List[ModifierApplyError]) =>
                logger.info(s"Application to state failed cause $e")
                val (newHis: History, newProgressInfo: ProgressInfo) =
                  history.reportModifierIsInvalid(modToApply)
                context.parent ! SemanticallyFailedModification(modToApply, e)
                UpdateInformation(newHis, u.state, modToApply.some, newProgressInfo.some, u.suffix)
            } else u
        }
        uf.failedMod match {
          case Some(_) =>
            uf.history.updateIdsForSyncInfo()
            updateState(uf.history, uf.state, uf.alternativeProgressInfo.get, uf.suffix, isLocallyGenerated)
          case None => (uf.history, uf.state, uf.suffix)
        }
      case Failure(e) =>
        context.parent ! RollbackFailed(branchingPointOpt)
        EncryApp.forceStopApplication(500, s"Rollback failed: $e")
    }
  }

  def applyModifier(modifier: PersistentModifier, isLocallyGenerated: Boolean = false): Unit =
    if (!nodeView.history.isModifierDefined(modifier.id)) {
      logger.debug(
        s"Start modifier ${modifier.encodedId} application of type ${modifier.modifierTypeId} to the history."
      )
      val startApplicationToTheHistory: Long = System.currentTimeMillis()
      context.parent ! StartApplyingModifier(modifier.id, modifier.modifierTypeId, System.currentTimeMillis())
      nodeView.history.append(modifier) match {
        case Right((historyBeforeStUpdate, progressInfo)) =>
          logger.info(
            s"Successfully applied modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} to the history. " +
              s"Time of applying is: ${(System.currentTimeMillis() - startApplicationToTheHistory) / 1000}s."
          )
          if (modifier.modifierTypeId == Header.modifierTypeId) historyBeforeStUpdate.updateIdsForSyncInfo()
          context.parent ! EndOfApplyingModifier(modifier.id)
          context.parent ! ModifierAppendedToHistory(modifier match {
            case _: Header  => true
            case _: Payload => false
          }, success = true)
          logger.info(
            s"Going to apply modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} to the state. " +
              s"Progress info is: $progressInfo."
          )
          if (progressInfo.toApply.nonEmpty) {
            val startPoint: Long = System.currentTimeMillis()
            logger.info(s"Progress info is non empty. To apply is: ${progressInfo.toApply.map(_.encodedId)}.")
            val (newHistory: History, newState: UtxoState, blocksApplied: Seq[PersistentModifier]) =
              updateState(historyBeforeStUpdate, nodeView.state, progressInfo, IndexedSeq(), isLocallyGenerated)
            if (newHistory.isHeadersChainSynced) context.parent ! HeaderChainIsSynced
            context.parent ! StateUpdating(System.currentTimeMillis() - startPoint)
            sendUpdatedInfoToMemoryPool(progressInfo.toRemove, progressInfo.toApply)
            if (progressInfo.chainSwitchingNeeded)
              nodeView.wallet.rollback(VersionTag !@@ progressInfo.branchPoint.get).get
            blocksApplied.foreach(nodeView.wallet.scanPersistent)
            logger.debug(s"Persistent modifier ${modifier.encodedId} was applied successfully.")
            newHistory.getBestHeader.foreach(context.parent ! BestHeaderInChain(_))
            if (newHistory.isFullChainSynced) {
              logger.info(s"BlockChain is synced on nvh at the height ${newHistory.getBestHeaderHeight}.")
              ModifiersCache.setChainSynced()
              context.parent ! FullBlockChainIsSynced
              context.system.eventStream.publish(FullBlockChainIsSynced)
            }
            updateNodeView(newHistory.some, newState.some, nodeView.wallet.some)
          } else {
            logger.info(s"Progress info is empty.")
            context.parent ! HeightStatistics(historyBeforeStUpdate.getBestHeaderHeight, nodeView.state.height)
            if (!isLocallyGenerated) requestDownloads(progressInfo, modifier.id.some)
            context.system.eventStream.publish(SemanticallySuccessfulModifier(modifier))
            updateNodeView(updatedHistory = historyBeforeStUpdate.some)
          }
        case Left(e: Throwable) =>
          logger.debug(s"Can't apply modifier ${modifier.encodedId}, contents: $modifier to history cause $e.")
          context.parent ! SyntacticallyFailedModification(modifier, List(HistoryApplyError(e.getMessage)))
      }
    } else logger.info(s"Trying to apply modifier ${modifier.encodedId} that's already in history.")

  def sendUpdatedInfoToMemoryPool(toRemove: Seq[PersistentModifier], toApply: Seq[PersistentModifier]): Unit = {
    val toRemoveTxs: IndexedSeq[Transaction] = toRemove
      .flatMap(extractTransactions)
      .toIndexedSeq
    val toApplyTxs: Vector[String] = toApply
      .flatMap(extractTransactions)
      .toVector
      .map(_.encodedId)
    val resultedTxs: IndexedSeq[Transaction] = toRemoveTxs.filterNot(tx => toApplyTxs.contains(tx.encodedId))
    if (resultedTxs.nonEmpty) context.parent ! RolledBackTransactions(resultedTxs)
  }

  def extractTransactions(mod: PersistentModifier): Seq[Transaction] = mod match {
    case b: Block   => b.payload.txs
    case p: Payload => p.txs
    case _          => Seq.empty[Transaction]
  }

  def genesisState: NodeView = {
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdir()
    val rootsDir: File = UtxoState.getRootsDir(settings)
    rootsDir.mkdir()
    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty.")
    val state: UtxoState = UtxoState.genesis(stateDir, rootsDir, settings, influxRef)
    val history: History = History.readOrGenerate(settings, ntp)
    val wallet: EncryWallet =
      EncryWallet.readOrGenerate(EncryWallet.getWalletDir(settings), EncryWallet.getKeysDir(settings), settings)
    NodeView(history, state, wallet)
  }

  def restoreState(influxRef: Option[ActorRef] = none): Option[NodeView] =
    if (History.getHistoryIndexDir(settings).listFiles.nonEmpty)
      try {
        val stateDir: File = UtxoState.getStateDir(settings)
        stateDir.mkdirs()
        val rootsDir: File = UtxoState.getRootsDir(settings)
        rootsDir.mkdir()
        val history: History = History.readOrGenerate(settings, ntp)
        val wallet: EncryWallet =
          EncryWallet.readOrGenerate(EncryWallet.getWalletDir(settings), EncryWallet.getKeysDir(settings), settings)
        val state: UtxoState = restoreConsistentState(
          UtxoState.create(stateDir, rootsDir, settings, influxRef),
          history,
          influxRef
        )
        history.updateIdsForSyncInfo()
        logger.info(s"History best block height: ${history.getBestBlockHeight}")
        logger.info(s"History best header height: ${history.getBestHeaderHeight}")
        NodeView(history, state, wallet).some
      } catch {
        case ex: Throwable =>
          logger.info(s"${ex.getMessage} during state restore. Recover from Modifiers holder!")
          new File(settings.directory).listFiles.foreach(dir => FileUtils.cleanDirectory(dir))
          genesisState.some
      } else {
      none
    }

  def getRecreatedState(
    version: Option[VersionTag] = none,
    digest: Option[ADDigest] = none,
    influxRef: Option[ActorRef]
  ): UtxoState = {
    val dir: File = UtxoState.getStateDir(settings)
    dir.mkdirs()
    dir.listFiles.foreach(_.delete())
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdirs()
    val rootsDir: File = UtxoState.getRootsDir(settings)
    rootsDir.mkdir()
    UtxoState.create(stateDir, rootsDir, settings, influxRef)
  }

  def restoreConsistentState(
    stateIn: UtxoState,
    history: History,
    influxRefActor: Option[ActorRef]
  ): UtxoState =
    (stateIn.version, history.getBestBlock, stateIn, stateIn.safePointHeight) match {
      case (stateId, None, _, _) if stateId sameElements Array.emptyByteArray =>
        logger.info(s"State and history are both empty on startup")
        stateIn
      case (_, None, _, _) =>
        logger.info(
          s"State and history are inconsistent." +
            s" History is empty on startup, rollback state to genesis."
        )
        getRecreatedState(influxRef = influxRefActor)
      case (_, Some(historyBestBlock), state: UtxoState, safePointHeight) =>
        val headerAtSafePointHeight = history.getBestHeaderAtHeight(safePointHeight)
        val (rollbackId, newChain)  = history.getChainToHeader(headerAtSafePointHeight, historyBestBlock.header)
        logger.info(
          s"State and history are inconsistent." +
            s" Going to rollback to ${rollbackId.map(Algos.encode)} and " +
            s"apply ${newChain.length} modifiers. State safe point: ${safePointHeight}. ${newChain.headers.head.height}. ${newChain.headers.last.height}"
        )
        val additionalBlocks =
          (state.safePointHeight + 1 to historyBestBlock.header.height).foldLeft(List.empty[Block]) {
            case (blocks, height) =>
              val headerAtHeight = history.getBestHeaderAtHeight(height).get
              val blockAtHeight  = history.getBlockByHeader(headerAtHeight).get
              blocks :+ blockAtHeight
          }
        logger.info(s"Qty of additional blocks: ${additionalBlocks.length}")
        rollbackId
          .map(_ => state.restore(additionalBlocks).get)
          .getOrElse(getRecreatedState(influxRef = influxRefActor))
    }

  override def close(): Unit = {
    nodeView.history.close()
    nodeView.state.close()
    nodeView.wallet.close()
  }
}

object NodeViewHolder {

  def toKey(id: ModifierId): mutable.WrappedArray.ofByte = new mutable.WrappedArray.ofByte(id)

  final case class UpdateHistoryReader(history: HistoryReader) extends AnyVal

  final case class NodeView(history: History, state: UtxoState, wallet: EncryWallet)

  object ReceivableMessages {
    final case class CreateAccountManagerFromSeed(seed: String)         extends AnyVal
    final case class LocallyGeneratedModifier(pmod: PersistentModifier) extends AnyVal
  }

  trait NodeViewHolderEvent

  trait NodeViewChange extends NodeViewHolderEvent

  case class RollbackFailed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

  case class RollbackSucceed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

  case class SyntacticallyFailedModification(modifier: PersistentNodeViewModifier, errors: List[ModifierApplyError])
      extends ModificationOutcome

  case class SemanticallyFailedModification(modifier: PersistentNodeViewModifier, errors: List[ModifierApplyError])
      extends ModificationOutcome

  case class SuccessfulTransaction(transaction: Transaction) extends ModificationOutcome

  case class SemanticallySuccessfulModifier(modifier: PersistentNodeViewModifier) extends ModificationOutcome

  case class GetDataFromCurrentView[HIS, MS, VL, A](f: CurrentView[HIS, MS, VL] => A)

  final case class DownloadRequest(
    modifierTypeId: ModifierTypeId,
    modifierIds: List[ModifierId]
  ) extends NodeViewHolderEvent

  final case class UpdateInformation(
    history: History,
    state: UtxoState,
    failedMod: Option[PersistentModifier],
    alternativeProgressInfo: Option[ProgressInfo],
    suffix: IndexedSeq[PersistentModifier]
  )

  def props(
    settings: EncryAppSettings,
    timeProvider: NetworkTimeProvider,
    influxRef: Option[ActorRef]
  ): Props = Props(new NodeViewHolder(settings, timeProvider, influxRef))
}
