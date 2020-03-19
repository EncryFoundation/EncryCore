package encry.nvg

import java.io.File

import akka.actor.{Actor, ActorRef, Props}
import cats.syntax.option.none
import com.typesafe.scalalogging.StrictLogging
import encry.settings.{EncryAppSettings, NodeSettings}
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.NodeViewErrors.ModifierApplyError
import encry.view.history.History
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ModifierId}

class NVHState(influxRef: Option[ActorRef], var state: UtxoState) extends Actor {

  override def receive: Receive = ???

}

object NVHState extends StrictLogging {

  sealed trait StateAction
  object StateAction {
    case class ModifierApplied(modifierId: ModifierId) extends StateAction
    case class Rollback(branchPoint: ModifierId) extends StateAction
    case class ValidationFailed(modifierId: ModifierId, errs: List[ModifierApplyError]) extends StateAction
  }

  //genesis state
  def props(settings: EncryAppSettings, branchPoint: ModifierId, influxRef: Option[ActorRef]): Props = {
    val stateDir: File = UtxoState.getStateDir(settings)
    val rootsDir: File = UtxoState.getRootsDir(settings)
    val state: UtxoState = UtxoState.genesis(stateDir, rootsDir, settings, influxRef)
    Props(new NVHState(influxRef, state))
  }

  //restoreConsistentState
  def props(settings: EncryAppSettings,
            branchPoint: ModifierId,
            bestBlock: Option[Block],
            influxRef: Option[ActorRef]): Props = {
    val stateDir: File = UtxoState.getStateDir(settings)
    val rootsDir: File = UtxoState.getRootsDir(settings)
    val state = UtxoState.create(stateDir, rootsDir, settings, influxRef)
    Props(new NVHState(influxRef, state))
  }

  def restoreConsistentState(stateIn: UtxoState,
                             history: History,
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
