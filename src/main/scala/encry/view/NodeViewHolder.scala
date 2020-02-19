package encry.view

import java.io.File
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.pattern._
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.EncryApp.{system, timeProvider}
import encry.api.http.DataHolderForApi
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.stats.StatsSender._
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.NodeViewHolder.ReceivableMessages._
import encry.view.NodeViewHolder._
import encry.view.fast.sync.SnapshotHolder.SnapshotManifest.ManifestId
import encry.view.fast.sync.SnapshotHolder._
import encry.view.history.storage.HistoryStorage
import encry.view.history.{History, HistoryHeadersProcessor, HistoryPayloadsProcessor}
import encry.view.mempool.MemoryPool.RolledBackTransactions
import encry.view.state.UtxoState
import encry.view.state.avlTree.AvlTree
import encry.view.wallet.EncryWallet
import io.iohk.iodb.ByteArrayWrapper
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ModifierId, ModifierTypeId}
import scala.collection.{IndexedSeq, Seq, mutable}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Try}

class NodeViewHolder(memoryPoolRef: ActorRef,
                     influxRef: Option[ActorRef],
                     dataHolder: ActorRef,
                     encrySettings: EncryAppSettings) extends Actor with StrictLogging with AutoCloseable {

  implicit val exCon: ExecutionContextExecutor = context.dispatcher

  case class NodeView(history: History, state: UtxoState, wallet: EncryWallet)

  var nodeView: NodeView = restoreState().getOrElse(genesisState(influxRef))
  context.system.actorSelection("/user/nodeViewSynchronizer") ! ChangedHistory(nodeView.history)

  dataHolder ! UpdatedHistory(nodeView.history)
  dataHolder ! ChangedState(nodeView.state)
  dataHolder ! DataHolderForApi.BlockAndHeaderInfo(nodeView.history.getBestHeader, nodeView.history.getBestBlock)

  influxRef.foreach(ref => context.system.scheduler.schedule(5.second, 5.second) {
    logger.info(s"send info. about ${nodeView.history.getBestHeaderHeight} | ${nodeView.history.getBestBlockHeight} | " +
      s"${nodeView.state.height} -> best header id ${nodeView.history.getBestHeader.map(_.encodedId)} ->" +
      s" best block id ${nodeView.history.getBestBlock.map(_.encodedId)}" +
      s" Best header at best block height ${nodeView.history.getBestBlock.flatMap(b =>
        nodeView.history.getBestHeaderAtHeight(b.header.height)
      ).map(l => l.encodedId -> Algos.encode(l.payloadId))}")
  })

  override def preStart(): Unit = logger.info(s"Node view holder started.")

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    reason.printStackTrace()
    System.exit(100)
  }

  context.system.scheduler.schedule(1.seconds, 10.seconds)(logger.info(s"Modifiers cache from NVH: " +
    s"${ModifiersCache.size}. Elems: ${ModifiersCache.cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")}"))

  override def postStop(): Unit = {
    logger.warn(s"Stopping NodeViewHolder...")
    nodeView.history.closeStorage()
  }

  var potentialManifestIds: List[ManifestId] = List.empty[ManifestId]

  override def receive: Receive = {
    case CreateAccountManagerFromSeed(seed) =>
      val newAccount = nodeView.wallet.addAccount(seed, encrySettings.wallet.map(_.password).get, nodeView.state)
      updateNodeView(updatedVault = newAccount.toOption)
      sender() ! newAccount
    case FastSyncFinished(state, wallet) =>
      logger.info(s"Node view holder got message FastSyncDoneAt. Started state replacing.")
      nodeView.state.tree.avlStorage.close()
      nodeView.wallet.close()
      FileUtils.deleteDirectory(new File(s"${encrySettings.directory}/tmpDirState"))
      FileUtils.deleteDirectory(new File(s"${encrySettings.directory}/keysTmp"))
      FileUtils.deleteDirectory(new File(s"${encrySettings.directory}/walletTmp"))
      logger.info(s"Updated best block in fast sync mod. Updated state height.")
      val newHistory = new History with HistoryHeadersProcessor with HistoryPayloadsProcessor {
        override val settings: EncryAppSettings = encrySettings
        override var isFullChainSynced: Boolean = settings.node.offlineGeneration
        override val timeProvider: NetworkTimeProvider = EncryApp.timeProvider
        override val historyStorage: HistoryStorage = nodeView.history.historyStorage
      }
      newHistory.fastSyncInProgress.fastSyncVal = false
      newHistory.blockDownloadProcessor.updateMinimalBlockHeightVar(nodeView.history.blockDownloadProcessor.minimalBlockHeight)
      newHistory.isHeadersChainSyncedVar = true
      updateNodeView(
        updatedHistory = Some(newHistory),
        updatedState = Some(state),
        updatedVault = Some(wallet)
      )
      system.actorSelection("/user/nodeViewSynchronizer") ! FastSyncDone
      logger.info(s"Fast sync finished successfully!")
    case RemoveRedundantManifestIds => potentialManifestIds = List.empty
    case ModifierFromRemote(mod) =>
      val isInHistory: Boolean = nodeView.history.isModifierDefined(mod.id)
      val isInCache: Boolean = ModifiersCache.contains(key(mod.id))
      if (isInHistory || isInCache)
        logger.info(s"Received modifier of type: ${mod.modifierTypeId}  ${Algos.encode(mod.id)} " +
          s"can't be placed into cache cause of: inCache: ${!isInCache}.")
      else ModifiersCache.put(key(mod.id), mod, nodeView.history)
      computeApplications()

    case lm: LocallyGeneratedModifier =>
      logger.debug(s"Start processing LocallyGeneratedModifier message on NVH.")
      val startTime = System.currentTimeMillis()
      logger.info(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      lm.pmod match {
        case block: Block =>
          pmodModify(block.header, isLocallyGenerated = true)
          pmodModify(block.payload, isLocallyGenerated = true)
        case anyMod =>
          pmodModify(anyMod, isLocallyGenerated = true)
      }
      logger.debug(s"Time processing of msg LocallyGeneratedModifier with mod of type ${lm.pmod.modifierTypeId}:" +
        s" with id: ${Algos.encode(lm.pmod.id)} -> ${System.currentTimeMillis() - startTime}")

    case GetDataFromCurrentView(f) =>
      f(CurrentView(nodeView.history, nodeView.state, nodeView.wallet)) match {
        case resultFuture: Future[_] => resultFuture.pipeTo(sender())
        case result => sender() ! result
      }

    case GetNodeViewChanges(history, state, _) =>
      if (history) sender() ! ChangedHistory(nodeView.history)
      if (state) sender() ! ChangedState(nodeView.state)

    case CompareViews(peer, modifierTypeId, modifierIds) =>
      logger.info(s"Start processing CompareViews message on NVH.")
      val startTime = System.currentTimeMillis()
      val ids: Seq[ModifierId] = modifierTypeId match {
        case _ => modifierIds
          .filterNot(mid => nodeView.history.isModifierDefined(mid) || ModifiersCache.contains(key(mid)))
      }
      if (modifierTypeId != Transaction.modifierTypeId) logger.debug(s"Got compare view message on NVH from ${peer.socketAddress}." +
        s" Type of requesting modifiers is: $modifierTypeId. Requesting ids size are: ${ids.size}." +
        s" Sending RequestFromLocal with ids to $sender." +
        s"\n Requesting ids are: ${ids.map(Algos.encode).mkString(",")}.")
      if (ids.nonEmpty && (modifierTypeId == Header.modifierTypeId || (nodeView.history.isHeadersChainSynced && modifierTypeId == Payload.modifierTypeId)))
        sender() ! RequestFromLocal(peer, modifierTypeId, ids)
      logger.debug(s"Time processing of msg CompareViews from $sender with modTypeId $modifierTypeId: ${System.currentTimeMillis() - startTime}")
    case SemanticallySuccessfulModifier(_) =>
    case msg => logger.error(s"Got strange message on nvh: $msg")
  }

  //todo refactor loop
  def computeApplications(): Unit = {
    val mods = ModifiersCache.popCandidate(nodeView.history)
    if (mods.nonEmpty) {
      logger.info(s"mods: ${mods.map(mod => Algos.encode(mod.id))}")
      mods.foreach(mod => pmodModify(mod))
      computeApplications()
    }
    else Unit
  }

  def key(id: ModifierId): mutable.WrappedArray.ofByte = new mutable.WrappedArray.ofByte(id)

  def updateNodeView(updatedHistory: Option[History] = None,
                     updatedState: Option[UtxoState] = None,
                     updatedVault: Option[EncryWallet] = None): Unit = {
    val newNodeView: NodeView = NodeView(updatedHistory.getOrElse(nodeView.history),
      updatedState.getOrElse(nodeView.state),
      updatedVault.getOrElse(nodeView.wallet))
    if (updatedHistory.nonEmpty) {
      system.actorSelection("/user/nodeViewSynchronizer") ! ChangedHistory(newNodeView.history)
      context.system.eventStream.publish(ChangedHistory(newNodeView.history))
    }
    if (updatedState.nonEmpty) context.system.eventStream.publish(ChangedState(newNodeView.state))
    nodeView = newNodeView
  }

  def requestDownloads(pi: ProgressInfo, previousModifier: Option[ModifierId] = None): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      if (tid != Transaction.modifierTypeId) logger.info(s"NVH trigger sending DownloadRequest to NVSH with type: $tid " +
        s"for modifier: ${Algos.encode(id)}. PrevMod is: ${previousModifier.map(Algos.encode)}.")
      if ((nodeView.history.isFullChainSynced && tid == Payload.modifierTypeId) || tid != Payload.modifierTypeId)
        system.actorSelection("/user/nodeViewSynchronizer")! DownloadRequest(tid, id, previousModifier)
      else logger.info(s"Ignore sending request for payload (${Algos.encode(id)}) from nvh because of nodeView.history.isFullChainSynced = false")
    }

  def trimChainSuffix(suffix: IndexedSeq[PersistentModifier], rollbackPoint: ModifierId):
  IndexedSeq[PersistentModifier] = {
    val idx: Int = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }

  @scala.annotation.tailrec
  private def updateState(history: History,
                          state: UtxoState,
                          progressInfo: ProgressInfo,
                          suffixApplied: IndexedSeq[PersistentModifier],
                          isLocallyGenerated: Boolean = false):
  (History, UtxoState, Seq[PersistentModifier]) = {
    logger.info(s"\nStarting updating state in updateState function!")
    if (!isLocallyGenerated) progressInfo.toApply.foreach {
      case header: Header => requestDownloads(progressInfo, Some(header.id))
      case _ => requestDownloads(progressInfo, None)
    }
    val branchingPointOpt: Option[VersionTag] = progressInfo.branchPoint.map(VersionTag !@@ _)
    val (stateToApplyTry: Try[UtxoState], suffixTrimmed: IndexedSeq[PersistentModifier]@unchecked) =
      if (progressInfo.chainSwitchingNeeded) {
        branchingPointOpt.map { branchPoint =>
          if (!state.version.sameElements(branchPoint)) {
            val branchPointHeight = history.getHeaderById(ModifierId !@@ branchPoint).get.height
            val additionalBlocks = (state.safePointHeight + 1 to branchPointHeight).foldLeft(List.empty[Block]){
              case (blocks, height) =>
                val headerAtHeight = history.getBestHeaderAtHeight(height).get
                val blockAtHeight = history.getBlockByHeader(headerAtHeight).get
                blocks :+ blockAtHeight
            }
            state.rollbackTo(branchPoint, additionalBlocks) -> trimChainSuffix(suffixApplied, ModifierId !@@ branchPoint)
          } else Success(state) -> IndexedSeq()
        }.getOrElse(Failure(new Exception("Trying to rollback when branchPoint is empty.")))
      } else Success(state) -> suffixApplied
    stateToApplyTry match {
      case Success(stateToApply) =>
        context.system.eventStream.publish(RollbackSucceed(branchingPointOpt))
        val u0: UpdateInformation = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
        val uf: UpdateInformation = progressInfo.toApply.foldLeft(u0) { case (u, modToApply) =>
          val saveRootNodesFlag = (history.getBestHeaderHeight - history.getBestBlockHeight - 1) < encrySettings.constants.MaxRollbackDepth * 2
          if (u.failedMod.isEmpty) u.state.applyModifier(modToApply, saveRootNodesFlag) match {
            case Right(stateAfterApply) =>
              influxRef.foreach(ref => modToApply match {
                case b: Block if history.isFullChainSynced => ref ! TransactionsInBlock(b.payload.txs.size)
                case _ =>
              })
              val newHis: History = history.reportModifierIsValid(modToApply)
              dataHolder ! DataHolderForApi.BlockAndHeaderInfo(newHis.getBestHeader, newHis.getBestBlock)
              modToApply match {
                case header: Header =>
                  val requiredHeight: Int = header.height - encrySettings.constants.MaxRollbackDepth
                  if (requiredHeight % encrySettings.constants.SnapshotCreationHeight == 0) {
                    newHis.lastAvailableManifestHeight = requiredHeight
                    logger.info(s"heightOfLastAvailablePayloadForRequest -> ${newHis.lastAvailableManifestHeight}")
                  }
                case _ =>
              }
              newHis.getHeaderOfBestBlock.foreach { header: Header =>
                val potentialManifestId: Array[Byte] = Algos.hash(stateAfterApply.tree.rootHash ++ header.id)
                val isManifestExists: Boolean = potentialManifestIds.exists(_.sameElements(potentialManifestId))
                val isCorrectCreationHeight: Boolean =
                  header.height % encrySettings.constants.SnapshotCreationHeight == 0
                val isGenesisHeader: Boolean = header.height == encrySettings.constants.GenesisHeight
                if (encrySettings.snapshotSettings.enableSnapshotCreation && newHis.isFullChainSynced &&
                  !isManifestExists && isCorrectCreationHeight && !isGenesisHeader) {
                  val startTime = System.currentTimeMillis()
                  logger.info(s"Start chunks creation for new snapshot")
                  import encry.view.state.avlTree.utils.implicits.Instances._
                  val chunks: List[SnapshotChunk] =
                    AvlTree.getChunks(
                      stateAfterApply.tree.rootNode,
                      currentChunkHeight = encrySettings.snapshotSettings.chunkDepth,
                      stateAfterApply.tree.avlStorage
                    )
                  system.actorSelection("/user/nodeViewSynchronizer") ! TreeChunks(chunks, potentialManifestId)
                  potentialManifestIds = ManifestId @@ potentialManifestId :: potentialManifestIds
                  logger.info(s"State tree successfully processed for snapshot. " +
                    s"Processing time is: ${(System.currentTimeMillis() - startTime) / 1000}s.")
                }
              }
              context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
              if (newHis.getBestHeaderId.exists(bestHeaderId =>
                  newHis.getBestBlockId.exists(bId => ByteArrayWrapper(bId) == ByteArrayWrapper(bestHeaderId))
                  )) newHis.isFullChainSynced = true
              influxRef.foreach { ref =>
                logger.info(s"send info 2. about ${newHis.getBestHeaderHeight} | ${newHis.getBestBlockHeight}")
                ref ! HeightStatistics(newHis.getBestHeaderHeight, stateAfterApply.height)
                val isBlock: Boolean = modToApply match {
                  case _: Block => true
                  case _: Payload => true
                  case _ => false
                }
                if (isBlock) ref ! ModifierAppendedToState(success = true)
              }
              UpdateInformation(newHis, stateAfterApply, None, None, u.suffix :+ modToApply)
            case Left(e) =>
              logger.info(s"Application to state failed cause $e")
              val (newHis: History, newProgressInfo: ProgressInfo) =
                history.reportModifierIsInvalid(modToApply)
              context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
              UpdateInformation(newHis, u.state, Some(modToApply), Some(newProgressInfo), u.suffix)
          } else u
        }
        uf.failedMod match {
          case Some(_) =>
            uf.history.updateIdsForSyncInfo()
            updateState(uf.history, uf.state, uf.alternativeProgressInfo.get, uf.suffix, isLocallyGenerated)
          case None => (uf.history, uf.state, uf.suffix)
        }
      case Failure(e) =>
        context.system.eventStream.publish(RollbackFailed(branchingPointOpt))
        EncryApp.forceStopApplication(500, s"Rollback failed: $e")
    }
  }

  def pmodModify(pmod: PersistentModifier, isLocallyGenerated: Boolean = false): Unit =
    if (!nodeView.history.isModifierDefined(pmod.id)) {
      logger.debug(s"\nStarting to apply modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} on nodeViewHolder to history.")
      val startAppHistory = System.currentTimeMillis()
      if (encrySettings.influxDB.isDefined) context.system
        .actorSelection("user/statsSender") !
        StartApplyingModifier(pmod.id, pmod.modifierTypeId, System.currentTimeMillis())
      nodeView.history.append(pmod) match {
          case Right((historyBeforeStUpdate, progressInfo)) =>
            logger.info(s"Successfully applied modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} on nodeViewHolder to history.")
            logger.debug(s"Time of applying to history SUCCESS is: ${System.currentTimeMillis() - startAppHistory}. modId is: ${pmod.encodedId}")
            if (pmod.modifierTypeId == Header.modifierTypeId) historyBeforeStUpdate.updateIdsForSyncInfo()
            influxRef.foreach { ref =>
              ref ! EndOfApplyingModifier(pmod.id)
              val isHeader: Boolean = pmod match {
                case _: Header => true
                case _: Payload => false
              }
              ref ! ModifierAppendedToHistory(isHeader, success = true)
            }
            if (historyBeforeStUpdate.fastSyncInProgress.fastSyncVal && pmod.modifierTypeId == Payload.modifierTypeId &&
              historyBeforeStUpdate.getBestBlockHeight >= historyBeforeStUpdate.lastAvailableManifestHeight) {
              logger.info(s"nodeView.history.getBestBlockHeight ${historyBeforeStUpdate.getBestBlockHeight}")
              logger.info(s"nodeView.history.heightOfLastAvailablePayloadForRequest ${historyBeforeStUpdate.lastAvailableManifestHeight}")
              historyBeforeStUpdate.getBestHeaderAtHeight(historyBeforeStUpdate.lastAvailableManifestHeight)
                .foreach { h =>
                  system.actorSelection("/user/nodeViewSynchronizer") ! RequiredManifestHeightAndId(
                    historyBeforeStUpdate.lastAvailableManifestHeight,
                    Algos.hash(h.stateRoot ++ h.id)
                  )
                }
            }
            logger.info(s"Going to apply modifications ${pmod.encodedId} of type ${pmod.modifierTypeId} on nodeViewHolder to the state: $progressInfo")
            if (progressInfo.toApply.nonEmpty) {
              logger.info(s"\n progress info non empty. To apply: ${progressInfo.toApply.map(mod => Algos.encode(mod.id))}")
              val startPoint: Long = System.currentTimeMillis()
              val (newHistory: History, newState: UtxoState, blocksApplied: Seq[PersistentModifier]) =
                updateState(historyBeforeStUpdate, nodeView.state, progressInfo, IndexedSeq(), isLocallyGenerated)
              if (newHistory.isHeadersChainSynced) system.actorSelection("/user/nodeViewSynchronizer") ! HeaderChainIsSynced
              if (encrySettings.influxDB.isDefined)
                context.actorSelection("/user/statsSender") ! StateUpdating(System.currentTimeMillis() - startPoint)
              sendUpdatedInfoToMemoryPool(progressInfo.toRemove)
              if (progressInfo.chainSwitchingNeeded)
                nodeView.wallet.rollback(VersionTag !@@ progressInfo.branchPoint.get).get
              blocksApplied.foreach(nodeView.wallet.scanPersistent)
              logger.debug(s"\nPersistent modifier ${pmod.encodedId} applied successfully")
              if (encrySettings.influxDB.isDefined) newHistory.getBestHeader.foreach(header =>
                context.actorSelection("/user/statsSender") ! BestHeaderInChain(header))
              if (newHistory.isFullChainSynced) {
                logger.debug(s"\nblockchain is synced on nvh on height ${newHistory.getBestHeaderHeight}!")
                ModifiersCache.setChainSynced()
                system.actorSelection("/user/nodeViewSynchronizer") ! FullBlockChainIsSynced
                system.actorSelection("/user/miner") ! FullBlockChainIsSynced
              }
              updateNodeView(Some(newHistory), Some(newState), Some(nodeView.wallet))
            } else {
              influxRef.foreach { ref =>
                logger.info(s"send info 3. about ${historyBeforeStUpdate.getBestHeaderHeight} | ${historyBeforeStUpdate.getBestBlockHeight}")
                ref ! HeightStatistics(historyBeforeStUpdate.getBestHeaderHeight, nodeView.state.height)
              }
              if (!isLocallyGenerated) requestDownloads(progressInfo, Some(pmod.id))
              context.system.eventStream.publish(SemanticallySuccessfulModifier(pmod))
              logger.info(s"\nProgress info is empty")
              updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
            }
          case Left(e) =>
            logger.debug(s"\nCan`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod)" +
              s" to history caused $e")
            context.system.eventStream.publish(SyntacticallyFailedModification(pmod, List(HistoryApplyError(e.getMessage))))
        }
    } else logger.info(s"\nTrying to apply modifier ${pmod.encodedId} that's already in history.")

  def sendUpdatedInfoToMemoryPool(toRemove: Seq[PersistentModifier]): Unit = {
    val rolledBackTxs: IndexedSeq[Transaction] = toRemove
      .flatMap(extractTransactions)
      .toIndexedSeq
    if (rolledBackTxs.nonEmpty)
      memoryPoolRef ! RolledBackTransactions(rolledBackTxs)
  }

  def extractTransactions(mod: PersistentModifier): Seq[Transaction] = mod match {
    case b: Block => b.payload.txs
    case p: Payload => p.txs
    case _ => Seq.empty[Transaction]
  }

  def genesisState(influxRef: Option[ActorRef] = None): NodeView = {
    val stateDir: File = UtxoState.getStateDir(encrySettings)
    stateDir.mkdir()
    val rootsDir: File = UtxoState.getRootsDir(encrySettings)
    rootsDir.mkdir()
    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty.")
    val state: UtxoState = UtxoState.genesis(stateDir, rootsDir, encrySettings, influxRef)
    val history: History = History.readOrGenerate(encrySettings, timeProvider)
    val wallet: EncryWallet =
      EncryWallet.readOrGenerate(EncryWallet.getWalletDir(encrySettings), EncryWallet.getKeysDir(encrySettings), encrySettings)
    NodeView(history, state, wallet)
  }

  def restoreState(influxRef: Option[ActorRef] = None): Option[NodeView] = if (History.getHistoryIndexDir(encrySettings).listFiles.nonEmpty)
    try {
      val stateDir: File = UtxoState.getStateDir(encrySettings)
      stateDir.mkdirs()
      val rootsDir: File = UtxoState.getRootsDir(encrySettings)
      rootsDir.mkdir()
      val history: History = History.readOrGenerate(encrySettings, timeProvider)
      val wallet: EncryWallet =
        EncryWallet.readOrGenerate(EncryWallet.getWalletDir(encrySettings), EncryWallet.getKeysDir(encrySettings), encrySettings)
      val state: UtxoState = restoreConsistentState(
        UtxoState.create(stateDir, rootsDir, encrySettings, influxRef), history, influxRef
      )
      history.updateIdsForSyncInfo()
      logger.info(s"History best block height: ${history.getBestBlockHeight}")
      logger.info(s"History best header height: ${history.getBestHeaderHeight}")
      Some(NodeView(history, state, wallet))
    } catch {
      case ex: Throwable =>
        logger.info(s"${ex.getMessage} during state restore. Recover from Modifiers holder!")
        new File(encrySettings.directory).listFiles.foreach(dir => FileUtils.cleanDirectory(dir))
        Some(genesisState(influxRef))
    } else {
    None
  }

  def getRecreatedState(version: Option[VersionTag] = None,
                        digest: Option[ADDigest] = None,
                        influxRef: Option[ActorRef]): UtxoState = {
    val dir: File = UtxoState.getStateDir(encrySettings)
    dir.mkdirs()
    dir.listFiles.foreach(_.delete())
    val stateDir: File = UtxoState.getStateDir(encrySettings)
    stateDir.mkdirs()
    val rootsDir: File = UtxoState.getRootsDir(encrySettings)
    rootsDir.mkdir()
    UtxoState.create(stateDir, rootsDir, encrySettings, influxRef)
  }

  def restoreConsistentState(stateIn: UtxoState, history: History, influxRefActor: Option[ActorRef]): UtxoState =
    (stateIn.version, history.getBestBlock, stateIn, stateIn.safePointHeight) match {
      case (stateId, None, _, _) if stateId sameElements Array.emptyByteArray =>
        logger.info(s"State and history are both empty on startup")
        stateIn
      case (_, None, _, _) =>
        logger.info(s"State and history are inconsistent." +
          s" History is empty on startup, rollback state to genesis.")
        getRecreatedState(influxRef = influxRefActor)
      case (_, Some(historyBestBlock), state: UtxoState, safePointHeight) =>
        val headerAtSafePointHeight = history.getBestHeaderAtHeight(safePointHeight)
        val (rollbackId, newChain) = history.getChainToHeader(headerAtSafePointHeight, historyBestBlock.header)
        logger.info(s"State and history are inconsistent." +
          s" Going to rollback to ${rollbackId.map(Algos.encode)} and " +
          s"apply ${newChain.length} modifiers")
        val additionalBlocks = (state.safePointHeight + 1 to historyBestBlock.header.height).foldLeft(List.empty[Block]){
          case (blocks, height) =>
            val headerAtHeight = history.getBestHeaderAtHeight(height).get
            val blockAtHeight = history.getBlockByHeader(headerAtHeight).get
            blocks :+ blockAtHeight
        }
        logger.info(s"Qty of additional blocks: ${additionalBlocks.length}")
        rollbackId.map(_ => state.restore(additionalBlocks).get)
          .getOrElse(getRecreatedState(influxRef = influxRefActor))
    }

  override def close(): Unit = {
    nodeView.history.close()
    nodeView.state.close()
    nodeView.wallet.close()
  }
}

object NodeViewHolder {

  final case class DownloadRequest(modifierTypeId: ModifierTypeId,
                                   modifierId: ModifierId,
                                   previousModifier: Option[ModifierId] = None) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL](history: HIS, state: MS, vault: VL)

  case class UpdateInformation(history: History,
                               state: UtxoState,
                               failedMod: Option[PersistentModifier],
                               alternativeProgressInfo: Option[ProgressInfo],
                               suffix: IndexedSeq[PersistentModifier])

  object ReceivableMessages {
    case class CreateAccountManagerFromSeed(seed: String)

    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean)

    case class GetDataFromCurrentView[HIS, MS, VL, A](f: CurrentView[HIS, MS, VL] => A)

    case object GetWallet

    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    final case class ModifierFromRemote(serializedModifiers: PersistentModifier)

    case class LocallyGeneratedModifier(pmod: PersistentModifier)

  }

  class NodeViewHolderPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
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