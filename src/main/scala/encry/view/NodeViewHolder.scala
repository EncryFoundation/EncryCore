package encry.view

import java.io.File
import java.nio.file.{Path, SimpleFileVisitor}

import encry.view.state.avlTree.utils.implicits.Instances._
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.pattern._
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.EncryApp.{miner, nodeViewSynchronizer, timeProvider}
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.{EncryAppSettings, LevelDBSettings}
import encry.stats.StatsSender._
import encry.storage.VersionalStorage
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.settings.EncryAppSettings
import encry.stats.StatsSender._
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.NodeViewHolder.ReceivableMessages._
import encry.view.NodeViewHolder._
import encry.view.fast.sync.SnapshotProcessor
import encry.view.fast.sync.SnapshotHolder.{FastSyncDone, FastSyncFinished, HeaderChainIsSynced, RequiredManifestHeightAndId, SnapshotChunk, TreeChunks}
import encry.view.history.History
import encry.view.mempool.MemoryPool.RolledBackTransactions
import encry.view.state.{UtxoState, _}
import encry.view.state.avlTree.AvlTree
import encry.view.wallet.EncryWallet
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import encry.view.history.History
import encry.view.mempool.MemoryPool.RolledBackTransactions
import encry.view.state._
import encry.view.wallet.EncryWallet
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ModifierId, ModifierTypeId}
import org.iq80.leveldb.Options

import scala.collection.{IndexedSeq, Seq, mutable}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Try}

class NodeViewHolder(memoryPoolRef: ActorRef,
                     influxRef: Option[ActorRef],
                     dataHolder: ActorRef,
                     settings: EncryAppSettings) extends Actor with StrictLogging with AutoCloseable {

  implicit val exCon: ExecutionContextExecutor = context.dispatcher

  case class NodeView(history: History, state: UtxoState, wallet: EncryWallet)

  var nodeView: NodeView = restoreState().getOrElse(genesisState)
  nodeViewSynchronizer ! ChangedHistory(nodeView.history)

  dataHolder ! UpdatedHistory(nodeView.history)
  dataHolder ! ChangedState(nodeView.state)

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

  override def receive: Receive = {
    case CreateAccountManagerFromSeed(seed) =>
      val newAccount = nodeView.wallet.addAccount(seed, settings.wallet.map(_.password).get, nodeView.state)
      updateNodeView(updatedVault = newAccount.toOption)
      sender() ! newAccount
    case FastSyncFinished(state) =>
      logger.info(s"Node view holder got message FastSyncDoneAt. Started state replacing.")
      nodeView.state.tree.storage.close()
      FileUtils.deleteDirectory(new File(s"${settings.directory}/tmpDirState"))
        nodeView.history.getBestHeaderAtHeight(state.height).foreach { h =>
          logger.info(s"Updated best block in fast sync mod. Updated state height.")
          nodeView.history.isHeadersChainSyncedVar = true
          nodeView.history.fastSyncInProgress = false
          ModifiersCache.finishFastSync()
          logger.info(s"Start wallet scanning")
          val wallet = nodeView.wallet.scanWalletFromUtxo(state, nodeView.wallet.propositions)
          logger.info(s"Finished wallet scanning")
          updateNodeView(
            updatedState = Some(state),
            updatedVault = Some(wallet)
          )
          nodeViewSynchronizer ! FastSyncDone
        }
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
      nodeViewSynchronizer ! ChangedHistory(newNodeView.history)
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
        nodeViewSynchronizer ! DownloadRequest(tid, id, previousModifier)
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
                          suffixApplied: IndexedSeq[PersistentModifier]):
  (History, UtxoState, Seq[PersistentModifier]) = {
    logger.info(s"\nStarting updating state in updateState function!")
    progressInfo.toApply.foreach {
      case header: Header => requestDownloads(progressInfo, Some(header.id))
      case _ => requestDownloads(progressInfo, None)
    }
    val branchingPointOpt: Option[VersionTag] = progressInfo.branchPoint.map(VersionTag !@@ _)
    val (stateToApplyTry: Try[UtxoState], suffixTrimmed: IndexedSeq[PersistentModifier]@unchecked) =
      if (progressInfo.chainSwitchingNeeded) {
        branchingPointOpt.map { branchPoint =>
          if (!state.version.sameElements(branchPoint))
            state.rollbackTo(branchPoint) -> trimChainSuffix(suffixApplied, ModifierId !@@ branchPoint)
          else Success(state) -> IndexedSeq()
        }.getOrElse(Failure(new Exception("Trying to rollback when branchPoint is empty.")))
      } else Success(state) -> suffixApplied
    stateToApplyTry match {
      case Success(stateToApply) =>
        context.system.eventStream.publish(RollbackSucceed(branchingPointOpt))
        val u0: UpdateInformation = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
        val uf: UpdateInformation = progressInfo.toApply.foldLeft(u0) { case (u, modToApply) =>
          if (u.failedMod.isEmpty) u.state.applyModifier(modToApply) match {
            case Right(stateAfterApply) =>
              influxRef.foreach(ref => modToApply match {
                case b: Block if history.isFullChainSynced => ref ! TransactionsInBlock(b.payload.txs.size)
                case _ =>
              })
              val newHis: History = history.reportModifierIsValid(modToApply)
              modToApply match {
                case header: Header =>
                  val requiredHeight: Int = header.height - settings.levelDB.maxVersions
                  if (requiredHeight % settings.snapshotSettings.newSnapshotCreationHeight == 0) {
                    newHis.getBestHeaderAtHeight(header.height - settings.levelDB.maxVersions).foreach { h =>
                      logger.info(s"Sent to snapshot holder new required manifest height $requiredHeight. " +
                        s"header id ${h.encodedId}, state root ${Algos.encode(h.stateRoot)}" +
                        s"\n\n\n header - $h \n\n\n")
                      newHis.heightOfLastAvailablePayloadForRequest = requiredHeight
                      logger.info(s"newHis.heightOfLastAvailablePayloadForRequest -> ${
                        newHis.heightOfLastAvailablePayloadForRequest
                      }")
                    }
                  }
                case _ =>
              }
              if (settings.snapshotSettings.enableSnapshotCreation && newHis.isFullChainSynced &&
                newHis.getBestBlock.exists { block =>
                block.header.height % settings.snapshotSettings.newSnapshotCreationHeight == 0 &&
                  block.header.height != settings.constants.GenesisHeight }) {
                val startTime = System.currentTimeMillis()
                logger.info(s"\n<<<<<<<||||||||START tree assembly on NVH||||||||||>>>>>>>>>>")
                import encry.view.state.avlTree.utils.implicits.Instances._
                newHis.getBestBlock.foreach { b =>
                  val chunks: List[SnapshotChunk] =
                    AvlTree.getChunks(
                      stateAfterApply.tree.rootNode,
                      currentChunkHeight = settings.snapshotSettings.chunkDepth,
                      stateAfterApply.tree.storage
                    )
                  val potentialManifestId: Array[Byte] = Algos.hash(stateAfterApply.tree.rootHash ++ b.id)
                  nodeViewSynchronizer ! TreeChunks(chunks, potentialManifestId)
                }
                logger.info(s"Processing time ${(System.currentTimeMillis() - startTime) / 1000}s")
                logger.info(s"<<<<<<<||||||||FINISH tree assembly on NVH||||||||||>>>>>>>>>>\n")
              }

              context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
              if (newHis.getBestHeaderId.exists(bestHeaderId =>
                  newHis.getBestBlockId.exists(bId => ByteArrayWrapper(bId) == ByteArrayWrapper(bestHeaderId))
                  )) newHis.isFullChainSynced = true
              influxRef.foreach { ref =>
                logger.info(s"send info 2. about ${newHis.getBestHeaderHeight} | ${newHis.getBestBlockHeight}")
                ref ! HeightStatistics(newHis.getBestHeaderHeight, stateAfterApply.height)
                val isBlock: Boolean = modToApply match {
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
          case Some(_) => updateState(uf.history, uf.state, uf.alternativeProgressInfo.get, uf.suffix)
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
      if (settings.influxDB.isDefined) context.system
        .actorSelection("user/statsSender") !
        StartApplyingModifier(pmod.id, pmod.modifierTypeId, System.currentTimeMillis())
      if (pmod.modifierTypeId == Payload.modifierTypeId && nodeView.history.fastSyncInProgress) {
        pmod match {
          case p: Payload =>
            nodeView.history.processPayloadFastSync(p)
            context.system.eventStream.publish(SemanticallySuccessfulModifier(pmod))
            if (nodeView.history.getBestBlockHeight >= nodeView.history.heightOfLastAvailablePayloadForRequest) {
              logger.info(s"nodeView.history.getBestBlockHeight ${nodeView.history.getBestBlockHeight}")
              logger.info(s"nodeView.history.heightOfLastAvailablePayloadForRequest ${nodeView.history.heightOfLastAvailablePayloadForRequest}")
              nodeView.history.getBestHeaderAtHeight(nodeView.history.heightOfLastAvailablePayloadForRequest)
                .foreach { h =>
                  nodeViewSynchronizer ! RequiredManifestHeightAndId(
                    nodeView.history.heightOfLastAvailablePayloadForRequest,
                    Algos.hash(h.stateRoot ++ h.id)
                  )
                }
            }
            influxRef.foreach { ref =>
              ref ! EndOfApplyingModifier(pmod.id)
              val isHeader: Boolean = pmod match {
                case _: Header => true
                case _: Payload => false
              }
              ref ! ModifierAppendedToHistory(isHeader, success = true)
              ref ! HeightStatistics(nodeView.history.getBestHeaderHeight, nodeView.history.getBestBlockHeight)
            }
        }
      } else {
        nodeView.history.append(pmod) match {
          case Right((historyBeforeStUpdate, progressInfo)) =>
            logger.info(s"Successfully applied modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} on nodeViewHolder to history.")
            logger.debug(s"Time of applying to history SUCCESS is: ${System.currentTimeMillis() - startAppHistory}. modId is: ${pmod.encodedId}")
            influxRef.foreach { ref =>
              ref ! EndOfApplyingModifier(pmod.id)
              val isHeader: Boolean = pmod match {
                case _: Header => true
                case _: Payload => false
              }
              ref ! ModifierAppendedToHistory(isHeader, success = true)
            }
            logger.info(s"Going to apply modifications ${pmod.encodedId} of type ${pmod.modifierTypeId} on nodeViewHolder to the state: $progressInfo")
            if (progressInfo.toApply.nonEmpty) {
              logger.info(s"\n progress info non empty. To apply: ${progressInfo.toApply.map(mod => Algos.encode(mod.id))}")
              val startPoint: Long = System.currentTimeMillis()
              val (newHistory: History, newState: UtxoState, blocksApplied: Seq[PersistentModifier]) =
                updateState(historyBeforeStUpdate, nodeView.state, progressInfo, IndexedSeq())
              if (newHistory.isHeadersChainSynced) nodeViewSynchronizer ! HeaderChainIsSynced
              if (settings.influxDB.isDefined)
                context.actorSelection("/user/statsSender") ! StateUpdating(System.currentTimeMillis() - startPoint)
              sendUpdatedInfoToMemoryPool(progressInfo.toRemove)
              if (progressInfo.chainSwitchingNeeded)
                nodeView.wallet.rollback(VersionTag !@@ progressInfo.branchPoint.get).get
              blocksApplied.foreach(nodeView.wallet.scanPersistent)
              logger.debug(s"\nPersistent modifier ${pmod.encodedId} applied successfully")
              if (settings.influxDB.isDefined) newHistory.getBestHeader.foreach(header =>
                context.actorSelection("/user/statsSender") ! BestHeaderInChain(header))
              if (newHistory.isFullChainSynced) {
                logger.debug(s"\nblockchain is synced on nvh on height ${newHistory.getBestHeaderHeight}!")
                ModifiersCache.setChainSynced()
                Seq(nodeViewSynchronizer, miner).foreach(_ ! FullBlockChainIsSynced)
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

  def genesisState: NodeView = {
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdir()
    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty.")
    val state: UtxoState = UtxoState.genesis(stateDir, settings)
    val history: History = History.readOrGenerate(settings, timeProvider)
    val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
    NodeView(history, state, wallet)
  }

  def restoreState(): Option[NodeView] = if (History.getHistoryIndexDir(settings).listFiles.nonEmpty)
    try {
      val stateDir: File = UtxoState.getStateDir(settings)
      stateDir.mkdirs()
      val history: History = History.readOrGenerate(settings, timeProvider)
      val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
      val state: UtxoState = restoreConsistentState(
        UtxoState.create(stateDir, settings), history
      )
      Some(NodeView(history, state, wallet))
    } catch {
      case ex: Throwable =>
        logger.info(s"${ex.getMessage} during state restore. Recover from Modifiers holder!")
        new File(settings.directory).listFiles.foreach(dir => FileUtils.cleanDirectory(dir))
        Some(genesisState)
    } else {
    None
  }

  def getRecreatedState(version: Option[VersionTag] = None, digest: Option[ADDigest] = None): UtxoState = {
    val dir: File = UtxoState.getStateDir(settings)
    dir.mkdirs()
    dir.listFiles.foreach(_.delete())
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdirs()
    UtxoState.create(stateDir, settings)
  }

  def restoreConsistentState(stateIn: UtxoState, history: History): UtxoState =
    (stateIn.version, history.getBestBlock, stateIn) match {
      case (stateId, None, _) if stateId sameElements Array.emptyByteArray =>
        logger.info(s"State and history are both empty on startup")
        stateIn
      case (stateId, Some(block), _) if stateId sameElements block.id =>
        logger.info(s"State and history have the same version ${Algos.encode(stateId)}, no recovery needed.")
        stateIn
      case (_, None, _) =>
        logger.info(s"State and history are inconsistent." +
          s" History is empty on startup, rollback state to genesis.")
        getRecreatedState()
      case (stateId, Some(historyBestBlock), state: UtxoState) =>
        val stateBestHeaderOpt = history.getHeaderById(ModifierId !@@ stateId) //todo naming
        val (rollbackId, newChain) = history.getChainToHeader(stateBestHeaderOpt, historyBestBlock.header)
        logger.info(s"State and history are inconsistent." +
          s" Going to rollback to ${rollbackId.map(Algos.encode)} and " +
          s"apply ${newChain.length} modifiers")
        val startState = rollbackId.map(id => state.rollbackTo(VersionTag !@@ id).get)
          .getOrElse(getRecreatedState())
        val toApply = newChain.headers.map { h =>
          history.getBlockByHeader(h) match {
            case Some(fb) => fb
            case None => throw new Exception(s"Failed to get full block for header $h")
          }
        }
        toApply.foldLeft(startState) { (s, m) => s.applyValidModifier(m) }
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