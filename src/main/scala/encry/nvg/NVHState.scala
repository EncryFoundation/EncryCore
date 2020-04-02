package encry.nvg

import java.io.File

import akka.actor.{Actor, ActorRef, Props}
import cats.syntax.option.none
import com.typesafe.scalalogging.StrictLogging
import encry.nvg.IntermediaryNVHView.IntermediaryNVHViewActions.RegisterState
import encry.nvg.NVHState.StateAction.{ApplyFailed, ApplyModifier, CreateTreeChunks, ModifierApplied, Restore, Rollback, RollbackTo, StateStarted}
import encry.nvg.fast.sync.SnapshotProcessor.SnapshotChunk
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.TransactionsInBlock
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.storage.{RootNodesStorage, VersionalStorage}
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.NodeViewErrors.ModifierApplyError
import encry.view.history.HistoryReader
import encry.view.state.UtxoState.logger
import encry.view.state.avlTree.AvlTree
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.view.state.{UtxoState, UtxoStateReader}
import io.iohk.iodb.LSMStore
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ModifierId}
import org.iq80.leveldb.Options

import scala.util.{Failure, Success, Try}

class NVHState(influxRef: Option[ActorRef], var historyReader: HistoryReader, settings: EncryAppSettings)
  extends Actor with StrictLogging {

  override def preStart(): Unit = context.parent ! StateStarted

  override def receive: Receive = awaitingInitialCommand

  def awaitingInitialCommand: Receive = {
    case Restore =>
      val newState = restoreState(settings, historyReader, influxRef).getOrElse(genesis(settings, influxRef))
      context.parent ! RegisterState(UtxoStateReader(newState))
      context.become(modifierApplying(newState))
    case RollbackTo(branchPoint, safePointHeight) =>
      val newState = rollback(branchPoint, safePointHeight)
      context.parent ! RegisterState(UtxoStateReader(newState))
      context.become(modifierApplying(newState))
  }

  def modifierApplying(state: UtxoState): Receive = {
    case ApplyModifier(modifier: PersistentModifier,
    saveRootNodesFlag: Boolean,
    isFullChainSynced: Boolean) =>
      state.applyModifier(modifier, saveRootNodesFlag) match {
        case Right(stateAfterApply) =>
          modifier match {
            case b: Block if isFullChainSynced => context.parent ! TransactionsInBlock(b.payload.txs.size)
            case _  =>
          }
          context.become(modifierApplying(stateAfterApply))
          logger.info(s"Successfully apply modifier: ${Algos.encode(modifier.id)} of type ${modifier.modifierTypeId}")
          context.parent ! UtxoStateReader(state)
          context.parent ! ModifierApplied(modifier)
        case Left(e: List[ModifierApplyError]) =>
          logger.info(s"Application to state failed cause $e")
          context.parent ! ApplyFailed(modifier, e)
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
  }

  def genesis(settings: EncryAppSettings, influxRef: Option[ActorRef]): UtxoState = {
    logger.info("Init genesis!")
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdir()
    val rootsDir: File = UtxoState.getRootsDir(settings)
    rootsDir.mkdir()
    UtxoState.genesis(stateDir, rootsDir, settings, influxRef)
  }

  def restoreState(settings: EncryAppSettings,
                   historyReader: HistoryReader,
                   influxRef: Option[ActorRef]): Option[UtxoState] =
    if (historyReader.getBestHeaderHeight != settings.constants.PreGenesisHeight) {
      Try {
        val stateDir: File = UtxoState.getStateDir(settings)
        stateDir.mkdirs()
        val rootsDir: File = UtxoState.getRootsDir(settings)
        rootsDir.mkdir()
        restoreConsistentState(
          UtxoState.create(stateDir, rootsDir, settings, influxRef),
          historyReader,
          influxRef,
          settings
        )
      } match {
        case fail: Failure[UtxoState] =>
          logger.info(s"${fail.exception.getMessage} during state restore. Recover from Modifiers holder!")
          new File(settings.directory).listFiles.foreach(dir => FileUtils.cleanDirectory(dir))
          fail.toOption
        case res: Success[UtxoState] => res.toOption
      }
    } else none

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

  def rollback(branchPoint: ModifierId, safePointHeight: Int): UtxoState = {
    val dir: File = UtxoState.getStateDir(settings)
    dir.mkdirs()
    dir.listFiles.foreach(_.delete())
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdirs()
    val rootsDir: File = UtxoState.getRootsDir(settings)
    rootsDir.mkdir()
    val branchPointHeight = historyReader.getHeaderById(ModifierId !@@ branchPoint).get.height
    val additionalBlocks = (safePointHeight + 1 to branchPointHeight).foldLeft(List.empty[Block]){
      case (blocks, height) =>
        val headerAtHeight = historyReader.getBestHeaderAtHeight(height).get
        val blockAtHeight = historyReader.getBlockByHeader(headerAtHeight).get
        blocks :+ blockAtHeight
    }
    val storage = settings.storage.state match {
      case VersionalStorage.IODB =>
        logger.info("Init state with iodb storage")
        IODBWrapper(new LSMStore(stateDir, keepVersions = settings.constants.DefaultKeepVersions, keySize = 33))
      case VersionalStorage.LevelDB =>
        logger.info("Init state with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(stateDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB.copy(keySize = 33), keySize = 33))
    }
    val rootStorage = {
      val levelDBInit = LevelDbFactory.factory.open(rootsDir, new Options)
      RootNodesStorage[StorageKey, StorageValue](levelDBInit, settings.constants.MaxRollbackDepth, rootsDir)
    }
    UtxoState
      .rollbackTo(VersionTag !@@ branchPoint, additionalBlocks, storage, rootStorage, settings.constants, influxRef)
      .get
  }
}

object NVHState extends StrictLogging {

  sealed trait StateAction
  object StateAction {
    case object StateStarted extends StateAction
    case class ModifierApplied(modifierId: PersistentModifier) extends StateAction
    case class Rollback(branchPoint: ModifierId) extends StateAction
    case class ApplyFailed(modifierId: PersistentModifier, errs: List[ModifierApplyError]) extends StateAction
    case class ApplyModifier(modifier: PersistentModifier,
                             saveRootNodesFlag: Boolean,
                             isFullChainSynced: Boolean) extends StateAction
    case object CreateTreeChunks extends StateAction
    case object Restore extends StateAction
    case class RollbackTo(branchPoint: ModifierId, safePointHeight: Int) extends StateAction
    case class TreeChunks(chunks: List[SnapshotChunk]) extends StateAction
  }

  def restoreProps(settings: EncryAppSettings,
                   historyReader: HistoryReader,
                   influxRef: Option[ActorRef]): Props = {
    Props(
      new NVHState(
        influxRef,
        historyReader,
        settings
      )
    )
  }
}
