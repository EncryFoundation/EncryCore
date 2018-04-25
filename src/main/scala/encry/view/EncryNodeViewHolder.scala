package encry.view

import akka.actor.{ActorRef, ActorSystem, Props}
import encry.EncryApp
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.history.block.payload.{EncryBlockPayload, EncryBlockPayloadSerializer}
import encry.modifiers.history.{ADProofSerializer, ADProofs}
import encry.modifiers.mempool.{EncryBaseTransaction, EncryTransactionSerializer}
import encry.modifiers.state.box.proposition.EncryProposition
import encry.settings.{Algos, EncryAppSettings}
import encry.view.history.{EncryHistory, EncrySyncInfo}
import encry.view.mempool.EncryMempool
import encry.view.state.{DigestState, EncryState, StateMode, UtxoState}
import encry.view.wallet.EncryWallet
import scorex.core._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{FailedTransaction, SuccessfulTransaction}
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest

import scala.util.{Failure, Success, Try}

abstract class EncryNodeViewHolder[StateType <: EncryState[StateType]](settings: EncryAppSettings,
                                                                       timeProvider: NetworkTimeProvider)
  extends NodeViewHolder[EncryProposition, EncryBaseTransaction, EncryPersistentModifier] {

  override val networkChunkSize: Int = settings.scorexSettings.network.networkChunkSize

  override type MS = StateType
  override type SI = EncrySyncInfo
  override type HIS = EncryHistory
  override type VL = EncryWallet
  override type MP = EncryMempool

  override lazy val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] = Map(
    EncryBlockHeader.modifierTypeId     -> EncryBlockHeaderSerializer,
    EncryBlockPayload.modifierTypeId    -> EncryBlockPayloadSerializer,
    ADProofs.modifierTypeId             -> ADProofSerializer,
    Transaction.ModifierTypeId          -> EncryTransactionSerializer
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

  override protected def txModify(tx: EncryBaseTransaction): Unit = {
    memoryPool().put(tx) match {
      case Success(newPool) =>
        log.debug(s"Unconfirmed transaction $tx added to the memory pool")
        val newVault = vault().scanOffchain(tx)
        updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
        context.system.eventStream.publish(SuccessfulTransaction[EncryProposition, EncryBaseTransaction](tx))

      case Failure(e) =>
        context.system.eventStream.publish(FailedTransaction[EncryProposition, EncryBaseTransaction](tx, e))
    }
  }

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (EncryHistory, MS, EncryWallet, EncryMempool) = {
    val stateDir = EncryState.getStateDir(settings)
    stateDir.mkdir()

    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty")

    val state = {
      if (settings.nodeSettings.stateMode.isDigest) EncryState.generateGenesisDigestState(stateDir, settings.nodeSettings)
      else EncryState.generateGenesisUtxoState(stateDir, Some(self))._1
    }.asInstanceOf[MS]

    //todo: ensure that history is in certain mode
    val history = EncryHistory.readOrGenerate(settings, timeProvider)

    val wallet = EncryWallet.readOrGenerate(settings)

    val memPool = EncryMempool.empty(settings, timeProvider)

    (history, state, wallet, memPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState: Option[NodeView] = if (!EncryHistory.getHistoryDir(settings).listFiles.isEmpty) {
    val history = EncryHistory.readOrGenerate(settings, timeProvider)
    val wallet = EncryWallet.readOrGenerate(settings)
    val memPool = EncryMempool.empty(settings, timeProvider)
    val state = restoreConsistentState(EncryState.readOrGenerate(settings, Some(self)).asInstanceOf[MS], history)
    Some((history, state, wallet, memPool))
  } else None

  private def getRecreatedState(version: Option[VersionTag] = None, digest: Option[ADDigest] = None): StateType = {
    val dir = EncryState.getStateDir(settings)
    dir.mkdirs()
    dir.listFiles.foreach(_.delete())

    {
      (version, digest) match {
        case (Some(_), Some(_)) if settings.nodeSettings.stateMode.isDigest =>
          DigestState.create(version, digest, dir, settings.nodeSettings)
        case _ =>
          EncryState.readOrGenerate(settings, Some(self))
      }
    }.asInstanceOf[StateType]
      .ensuring(_.rootHash sameElements digest.getOrElse(EncryState.afterGenesisStateDigest), "State root is incorrect")
  }

  private def restoreConsistentState(stateIn: StateType, history: EncryHistory): StateType = Try {
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
        val toApply = newChain.headers.map{h =>
          history.getBlock(h) match {
            case Some(fb) => fb
            case None => throw new Error(s"Failed to get full block for header $h")
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

private[view] class DigestNodeViewHolder(settings: EncryAppSettings, timeProvider: NetworkTimeProvider)
  extends EncryNodeViewHolder[DigestState](settings, timeProvider)

private[view] class UtxoNodeViewHolder(settings: EncryAppSettings, timeProvider: NetworkTimeProvider)
  extends EncryNodeViewHolder[UtxoState](settings, timeProvider)

/** This class guarantees to its inheritors the creation of correct instance of [[EncryNodeViewHolder]]
  *  for the given instance of [[StateMode]]
  */
sealed abstract class EncryNodeViewProps[ST <: StateMode, S <: EncryState[S], N <: EncryNodeViewHolder[S]]
(implicit ev: StateMode.Evidence[ST, S]) {
  def apply(settings: EncryAppSettings, timeProvider: NetworkTimeProvider, digestType:ST): Props
}

object DigestNodeViewProps extends EncryNodeViewProps[StateMode.DigestMode, DigestState, DigestNodeViewHolder] {
  def apply(settings: EncryAppSettings, timeProvider: NetworkTimeProvider, digestType: StateMode.DigestMode): Props =
    Props.create(classOf[DigestNodeViewHolder], settings, timeProvider)
}

object UtxoNodeViewProps extends EncryNodeViewProps[StateMode.UtxoMode, UtxoState, UtxoNodeViewHolder]  {
  def apply(settings: EncryAppSettings, timeProvider: NetworkTimeProvider, digestType: StateMode.UtxoMode): Props =
    Props.create(classOf[UtxoNodeViewHolder], settings, timeProvider)
}

object EncryNodeViewHolderRef {

  def props(settings: EncryAppSettings, timeProvider: NetworkTimeProvider): Props =
    settings.nodeSettings.stateMode match {
      case digestType @ StateMode.Digest => DigestNodeViewProps(settings, timeProvider, digestType)
      case utxoType @ StateMode.Utxo => UtxoNodeViewProps(settings, timeProvider, utxoType)
    }

  def apply(settings: EncryAppSettings, timeProvider: NetworkTimeProvider)(implicit system: ActorSystem): ActorRef =
    system.actorOf(props(settings, timeProvider))
}
