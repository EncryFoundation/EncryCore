package encry.nvg

import java.io.File

import akka.actor.{Actor, ActorRef, Props}
import cats.syntax.option.none
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.BlockAndHeaderInfo
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.nvg.IntermediaryNVHView.IntermediaryNVHViewActions.RegisterState
import encry.nvg.NVHState.StateAction.{ApplyFailed, ApplyModifier, CreateTreeChunks, ModifierApplied}
import encry.nvg.NodeViewHolder.{SemanticallyFailedModification, SemanticallySuccessfulModifier, UpdateInformation}
import encry.nvg.fast.sync.SnapshotProcessor.SnapshotManifest.ManifestId
import encry.nvg.fast.sync.SnapshotProcessor.{SnapshotChunk, TreeChunks}
import encry.settings.{EncryAppSettings, NodeSettings}
import encry.stats.StatsSender.{HeightStatistics, ModifierAppendedToState, TransactionsInBlock}
import encry.storage.{RootNodesStorage, VersionalStorage}
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.NodeViewErrors.ModifierApplyError
import encry.view.history.{History, HistoryReader}
import encry.view.state.UtxoState
import encry.view.state.UtxoState.logger
import encry.view.state.avlTree.AvlTree
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ModifierId}
import org.encryfoundation.common.utils.constants.Constants
import org.iq80.leveldb.Options
import encry.view.state.avlTree.utils.implicits.Instances._

import scala.util.Try

class NVHState(influxRef: Option[ActorRef], var state: UtxoState, settings: EncryAppSettings)
  extends Actor with StrictLogging with AutoCloseable {

  override def preStart(): Unit = context.parent ! RegisterState

  override def receive: Receive = {
    case ApplyModifier(modifier: PersistentModifier,
                       saveRootNodesFlag: Boolean,
                       isFullChainSynced: Boolean) =>
      state.applyModifier(modifier, saveRootNodesFlag) match {
        case Right(stateAfterApply) =>
          modifier match {
            case b: Block if isFullChainSynced => context.parent ! TransactionsInBlock(b.payload.txs.size)
            case _                                     =>
          }
          state = stateAfterApply
          logger.info(s"Successfully apply modifier: ${Algos.encode(modifier.id)} of type ${modifier.modifierTypeId}")
          context.parent ! ModifierApplied(modifier.id)
        case Left(e: List[ModifierApplyError]) =>
          logger.info(s"Application to state failed cause $e")
          context.parent ! ApplyFailed(modifier.id, e)
      }
    case CreateTreeChunks =>
      context.parent ! AvlTree.getChunks(
        state.tree.rootNode,
        currentChunkHeight = settings.snapshotSettings.chunkDepth,
        state.tree.avlStorage
      )
  }

  override def postStop(): Unit = {
    logger.info("Close state!")
    state.close()
  }

  override def close(): Unit = state.close()
}

object NVHState extends StrictLogging {

  import encry.view.state.avlTree.utils.implicits.Instances._

  sealed trait StateAction
  object StateAction {
    case class ModifierApplied(modifierId: ModifierId) extends StateAction
    case class Rollback(branchPoint: ModifierId) extends StateAction
    case class ApplyFailed(modifierId: ModifierId, errs: List[ModifierApplyError]) extends StateAction
    case class ApplyModifier(modifier: PersistentModifier,
                             saveRootNodesFlag: Boolean,
                             isFullChainSynced: Boolean) extends StateAction
    case object CreateTreeChunks extends StateAction
    case class TreeChunks(chunks: List[SnapshotChunk]) extends StateAction
  }

  //genesis state
  def genesisProps(settings: EncryAppSettings, influxRef: Option[ActorRef]): Props = {
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdir()
    val rootsDir: File = UtxoState.getRootsDir(settings)
    rootsDir.mkdir()
    val state: UtxoState = UtxoState.genesis(stateDir, rootsDir, settings, influxRef)
    Props(new NVHState(influxRef, state, settings))
  }

  //restoreConsistentState
  def restoreConsistentStateProps(settings: EncryAppSettings,
                                  historyReader: HistoryReader,
                                  influxRef: Option[ActorRef]): Try[Props] = {
    Try {
      val stateDir: File = UtxoState.getStateDir(settings)
      stateDir.mkdirs()
      val rootsDir: File = UtxoState.getRootsDir(settings)
      rootsDir.mkdir()
      val state: UtxoState = restoreConsistentState(
        UtxoState.create(stateDir, rootsDir, settings, influxRef),
        historyReader,
        influxRef,
        settings
      )
      Props(new NVHState(influxRef, state, settings))
    }
  }

  //rollback
  def props(settings: EncryAppSettings,
            influxRef: Option[ActorRef],
            safePointHeight: Int,
            branchPoint: VersionTag,
            historyReader: HistoryReader,
            constants: Constants): Props = {
    val branchPointHeight: Int = historyReader.getHeaderById(ModifierId !@@ branchPoint).get.height
    val additionalBlocks: List[Block] =
      (safePointHeight + 1 to branchPointHeight).foldLeft(List.empty[Block]) {
        case (blocks: List[Block], height: Int) =>
          val headerAtHeight: Header = historyReader.getBestHeaderAtHeight(height).get
          val blockAtHeight: Block   = historyReader.getBlockByHeader(headerAtHeight).get
          blocks :+ blockAtHeight
      }
    val dir: File = UtxoState.getStateDir(settings)
    dir.mkdirs()
    dir.listFiles.foreach(_.delete())
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdirs()
    val rootsDir: File = UtxoState.getRootsDir(settings)
    rootsDir.mkdir()
    val versionalStorage = settings.storage.state match {
      case VersionalStorage.IODB =>
        logger.info("Init state with iodb storage")
        IODBWrapper(new LSMStore(stateDir, keepVersions = settings.constants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        logger.info("Init state with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(stateDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB.copy(keySize = 33), keySize = 33))
    }
    val rootStorage = {
      val levelDBInit = LevelDbFactory.factory.open(rootsDir, new Options)
      RootNodesStorage[StorageKey, StorageValue](levelDBInit, settings.constants.MaxRollbackDepth, rootsDir)
    }
    val state: UtxoState = UtxoState.rollbackTo(
      branchPoint,
      additionalBlocks,
      versionalStorage,
      rootStorage,
      constants,
      influxRef
    ).get
    Props(new NVHState(influxRef, state, settings))
  }

  def restoreConsistentState(stateIn: UtxoState,
                             history: HistoryReader,
                             influxRefActor: Option[ActorRef],
                             appSettings: EncryAppSettings): UtxoState =
    (stateIn.version, history.getBestBlock, stateIn, stateIn.safePointHeight) match {
      case (stateId, None, _, _) if stateId sameElements Array.emptyByteArray =>
        logger.info(s"State and history are both empty on startup")
        stateIn
      case (_, None, _, _) =>
        logger.info(
          s"State and history are inconsistent." +
            s" History is empty on startup, rollback state to genesis."
        )
        getRecreatedState(influxRef = influxRefActor, settings = appSettings)
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
          .getOrElse(getRecreatedState(influxRef = influxRefActor, settings = appSettings))
    }

  def getRecreatedState(version: Option[VersionTag] = none,
                        digest: Option[ADDigest] = none,
                        influxRef: Option[ActorRef],
                        settings: EncryAppSettings): UtxoState = {
    val dir: File = UtxoState.getStateDir(settings)
    dir.mkdirs()
    dir.listFiles.foreach(_.delete())
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdirs()
    val rootsDir: File = UtxoState.getRootsDir(settings)
    rootsDir.mkdir()
    UtxoState.create(stateDir, rootsDir, settings, influxRef)
  }
}
