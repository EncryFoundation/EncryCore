package encry.view

import java.io.File

import akka.actor.Props
import encry.EncryApp
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.history.block.payload.{EncryBlockPayload, EncryBlockPayloadSerializer}
import encry.modifiers.history.{ADProofSerializer, ADProofs}
import encry.modifiers.mempool.{EncryBaseTransaction, EncryTransactionSerializer}
import encry.modifiers.state.box.proposition.EncryProposition
import encry.settings.Algos
import encry.view.history.{EncryHistory, EncrySyncInfo}
import encry.view.mempool.EncryMempool
import encry.view.state.{DigestState, EncryState, StateMode, UtxoState}
import encry.view.wallet.EncryWallet
import encry.EncryApp.{encrySettings, timeProvider}
import scorex.core._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{FailedTransaction, SuccessfulTransaction}
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.crypto.authds.ADDigest

import scala.util.{Failure, Success, Try}

class EncryNodeViewHolder[StateType <: EncryState[StateType]]
  extends NodeViewHolder[EncryProposition, EncryBaseTransaction, EncryPersistentModifier] {

  override val networkChunkSize: Int = encrySettings.scorexSettings.network.networkChunkSize

  override type MS = StateType
  override type SI = EncrySyncInfo
  override type HIS = EncryHistory
  override type VL = EncryWallet
  override type MP = EncryMempool

  override lazy val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] = Map(
    EncryBlockHeader.modifierTypeId -> EncryBlockHeaderSerializer,
    EncryBlockPayload.modifierTypeId -> EncryBlockPayloadSerializer,
    ADProofs.modifierTypeId -> ADProofSerializer,
    Transaction.ModifierTypeId -> EncryTransactionSerializer
  )

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100)
  }

  override def postStop(): Unit = {
    log.warn("Stopping EncryNodeViewHolder")
    history().closeStorage()
    minimalState().closeStorage()
  }

  override protected def txModify(tx: EncryBaseTransaction): Unit = memoryPool().put(tx) match {
    case Success(newPool) =>
      log.debug(s"Unconfirmed transaction $tx added to the memory pool")
      val newVault = vault().scanOffchain(tx)
      updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
      context.system.eventStream.publish(SuccessfulTransaction[EncryProposition, EncryBaseTransaction](tx))
    case Failure(e) =>
      context.system.eventStream.publish(FailedTransaction[EncryProposition, EncryBaseTransaction](tx, e))
  }

  override protected def genesisState: (EncryHistory, MS, EncryWallet, EncryMempool) = {
    val stateDir: File = EncryState.getStateDir(encrySettings)
    stateDir.mkdir()

    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty")

    val state: StateType = {
      if (encrySettings.nodeSettings.stateMode.isDigest) EncryState.generateGenesisDigestState(stateDir, encrySettings.nodeSettings)
      else EncryState.generateGenesisUtxoState(stateDir, Some(self))._1
    }.asInstanceOf[MS]

    val history: EncryHistory = EncryHistory.readOrGenerate(encrySettings, timeProvider)

    val wallet: EncryWallet = EncryWallet.readOrGenerate(encrySettings)

    val memPool: EncryMempool = EncryMempool.empty(encrySettings, timeProvider)

    (history, state, wallet, memPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState(): Option[NodeView] = if (!EncryHistory.getHistoryDir(encrySettings).listFiles.isEmpty) {
    val history: EncryHistory = EncryHistory.readOrGenerate(encrySettings, timeProvider)
    val wallet: EncryWallet = EncryWallet.readOrGenerate(encrySettings)
    val memPool: EncryMempool = EncryMempool.empty(encrySettings, timeProvider)
    val state: StateType = restoreConsistentState(EncryState.readOrGenerate(encrySettings, Some(self)).asInstanceOf[MS], history)
    Some((history, state, wallet, memPool))
  } else None

  def getRecreatedState(version: Option[VersionTag] = None, digest: Option[ADDigest] = None): StateType = {
    val dir: File = EncryState.getStateDir(encrySettings)
    dir.mkdirs()
    dir.listFiles.foreach(_.delete())

    {
      (version, digest) match {
        case (Some(_), Some(_)) if encrySettings.nodeSettings.stateMode.isDigest =>
          DigestState.create(version, digest, dir, encrySettings.nodeSettings)
        case _ =>
          EncryState.readOrGenerate(encrySettings, Some(self))
      }
    }.asInstanceOf[StateType]
      .ensuring(_.rootHash sameElements digest.getOrElse(EncryState.afterGenesisStateDigest), "State root is incorrect")
  }

  def restoreConsistentState(stateIn: StateType, history: EncryHistory): StateType = Try {
    (stateIn.version, history.bestBlockOpt, stateIn) match {
      case (stateId, None, _) if stateId sameElements EncryState.genesisStateVersion =>
        log.debug("State and history are both empty on startup")
        stateIn
      case (stateId, Some(block), _) if stateId sameElements block.id =>
        log.debug(s"State and history have the same version ${Algos.encode(stateId)}, no recovery needed.")
        stateIn
      case (_, None, _) =>
        log.debug("State and history are inconsistent. History is empty on startup, rollback state to genesis.")
        getRecreatedState()
      case (_, Some(bestBlock), _: DigestState) =>
        // Update state.digest.
        log.debug(s"State and history are inconsistent. Going to switch state to version ${bestBlock.encodedId}")
        getRecreatedState(Some(VersionTag @@ bestBlock.id), Some(bestBlock.header.stateRoot))
      case (stateId, Some(historyBestBlock), state: StateType@unchecked) =>
        val stateBestHeaderOpt = history.typedModifierById[EncryBlockHeader](ModifierId @@ stateId)
        val (rollbackId, newChain) = history.getChainToHeader(stateBestHeaderOpt, historyBestBlock.header)
        log.debug(s"State and history are inconsistent. Going to rollback to ${rollbackId.map(Algos.encode)} and " +
          s"apply ${newChain.length} modifiers")
        val startState = rollbackId.map(id => state.rollbackTo(VersionTag @@ id).get)
          .getOrElse(getRecreatedState())
        val toApply = newChain.headers.map { h =>
          history.getBlock(h) match {
            case Some(fb) => fb
            case None => throw new Exception(s"Failed to get full block for header $h")
          }
        }
        toApply.foldLeft(startState) { (s, m) =>
          s.applyModifier(m).get
        }
    }
  }.recoverWith { case e =>
    log.error("Failed to recover state.", e)
    EncryApp.forceStopApplication(500)
  }.get
}

object EncryNodeViewHolder {

  def props(): Props = encrySettings.nodeSettings.stateMode match {
    case digestType@StateMode.Digest => Props(classOf[EncryNodeViewHolder[DigestState]])
    case utxoType@StateMode.Utxo => Props(classOf[EncryNodeViewHolder[UtxoState]])
  }
}