package encry.nvg.nvhg

import java.io.File

import akka.actor.{ Actor, ActorRef }
import cats.syntax.option._
import com.typesafe.scalalogging.StrictLogging
import encry.nvg.ModifiersCache
import encry.nvg.ModifiersValidator.ValidatedModifier
import encry.nvg.nvhg.NodeViewHolder.NodeView
import encry.nvg.nvhg.NodeViewHolderController.{ ApplicableModifier, GetNewApplicable, HistoryIsReady, StateIsReady }
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.NetworkTimeProvider
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADDigest

import scala.collection.mutable

class NodeViewHolderController(
  settings: EncryAppSettings,
  ntp: NetworkTimeProvider,
  influxRef: Option[ActorRef]
) extends Actor
    with StrictLogging {

  var nodeView: NodeView = restoreState().getOrElse(genesisState)

  val historyApplier: ActorRef = Actor.noSender

  val stateApplier: ActorRef = Actor.noSender

  override def receive: Receive = mainReceive.orElse(processModifierFromNetwork)

  def mainReceive: Receive = {
    case GetNewApplicable =>
      val mod: List[PersistentModifier] = ModifiersCache.popCandidate(nodeView.history)
      mod.headOption.foreach { mod: PersistentModifier =>
        historyApplier ! ApplicableModifier(mod)
        stateApplier ! ApplicableModifier(mod)
        logger.info(s"Applicable modifier ${mod.encodedId} of type ${mod.modifierTypeId} sent to history and state.")
        context.become(
          awaitApplication(historyIsReady = false, stateIsReady = false).orElse(processModifierFromNetwork)
        )
      }
  }

  def awaitApplication(historyIsReady: Boolean, stateIsReady: Boolean): Receive = {
    case HistoryIsReady =>
      if (stateIsReady) {
        self ! GetNewApplicable
        context.become(mainReceive.orElse(processModifierFromNetwork))
      } else context.become(awaitApplication(historyIsReady = true, stateIsReady).orElse(processModifierFromNetwork))
    case StateIsReady =>
      if (historyIsReady) {
        self ! GetNewApplicable
        context.become(mainReceive.orElse(processModifierFromNetwork))
      } else context.become(awaitApplication(historyIsReady, stateIsReady = true).orElse(processModifierFromNetwork))
  }

  def processModifierFromNetwork: Receive = {
    case ValidatedModifier(modifier: PersistentModifier) =>
      val wrappedKey: mutable.WrappedArray.ofByte = NodeViewHolder.toKey(modifier.id)
      val isInHistory: Boolean                    = nodeView.history.isModifierDefined(modifier.id)
      val isInCache: Boolean                      = ModifiersCache.contains(wrappedKey)
      if (isInHistory || isInCache)
        logger.info(
          s"Modifier ${modifier.encodedId} can't be placed into the cache cause: " +
            s"contains in cache: $isInCache, contains in history: $isInHistory."
        )
      else {
        ModifiersCache.put(wrappedKey, modifier, nodeView.history)
        self ! GetNewApplicable
      }
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
}

object NodeViewHolderController {

  final case class ApplicableModifier(modifier: PersistentModifier)
  case object HistoryIsReady
  case object StateIsReady
  case object GetNewApplicable
}
