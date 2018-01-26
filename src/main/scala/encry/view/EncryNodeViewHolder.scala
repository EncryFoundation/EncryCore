package encry.view

import akka.actor.{ActorRef, ActorSystem, Props}
import encry.EncryApp
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.history.block.payload.{EncryBlockPayload, EncryBlockPayloadSerializer}
import encry.modifiers.history.{ADProofSerializer, ADProofs}
import encry.modifiers.mempool.{CoinbaseTransactionSerializer, EncryBaseTransaction}
import encry.settings.EncryAppSettings
import encry.view.history.{EncryHistory, EncrySyncInfo}
import encry.view.mempool.EncryMempool
import encry.view.state.{DigestState, EncryState, UtxoState}
import encry.view.wallet.EncryWallet
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, ModifierTypeId, NodeViewHolder, NodeViewModifier}

import scala.util.{Failure, Success}

abstract class EncryNodeViewHolder[StateType <: EncryState[StateType]](settings: EncryAppSettings,
                                                                       timeProvider: NetworkTimeProvider)
  extends NodeViewHolder[Proposition, EncryBaseTransaction, EncryPersistentModifier] {

  // TODO: `settings.scorexSettings.network.networkChunkSize` should be used here.
  override val networkChunkSize: Int = 400

  override type MS = StateType
  override type SI = EncrySyncInfo
  override type HIS = EncryHistory
  override type VL = EncryWallet
  override type MP = EncryMempool

  // TODO: What about transactions of different types?
  // TODO: We're likely to have to implement `modifierSerializer` item for each tx type.
  override lazy val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] = Map(
    EncryBlockHeader.modifierTypeId     -> EncryBlockHeaderSerializer,
    EncryBlockPayload.modifierTypeId    -> EncryBlockPayloadSerializer,
    ADProofs.modifierTypeId             -> ADProofSerializer,
    EncryBaseTransaction.ModifierTypeId -> CoinbaseTransactionSerializer
  )

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100)
  }

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (EncryHistory, MS, EncryWallet, EncryMempool) = {
    val stateDir = EncryState.stateDir(settings)
    stateDir.mkdir()

    val idxDir = EncryState.indexDir(settings)
    idxDir.mkdirs()

    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty")

    val state = {
      if (settings.nodeSettings.ADState) EncryState.generateGenesisDigestState(stateDir, settings.nodeSettings)
      else EncryState.genGenesisUtxoState(stateDir, idxDir, Some(self))._1
    }.asInstanceOf[MS]

    //todo: ensure that history is in certain mode
    val history = EncryHistory.readOrGenerate(settings)

    val wallet = EncryWallet.readOrGenerate(settings)

    val memPool = EncryMempool.empty(settings, timeProvider)

    (history, state, wallet, memPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState: Option[NodeView] = {
    EncryState.readOrGenerate(settings, Some(self)).map { stateIn =>
      //todo: ensure that history is in certain mode
      val history = EncryHistory.readOrGenerate(settings)
      val wallet = EncryWallet.readOrGenerate(settings)
      val memPool = EncryMempool.empty(settings, timeProvider)
      val state = restoreConsistentState(stateIn.asInstanceOf[MS], history)
      (history, state, wallet, memPool)
    }
  }

  private def restoreConsistentState(state: StateType, history: EncryHistory): StateType = {
    if (history.bestFullBlockIdOpt.isEmpty) {
      state
    } else {
      val stateBestBlockId = if (state.version sameElements EncryState.genesisStateVersion) None else Some(state.version)
      val hFrom = stateBestBlockId.flatMap(id => history.typedModifierById[EncryBlockHeader](ModifierId @@ id))
      val fbFrom = hFrom.flatMap(h => history.getFullBlock(h))
      history.fullBlocksAfter(fbFrom).map { toApply =>
        if (toApply.nonEmpty) {
          log.info(s"State and History are inconsistent on startup. Going to apply ${toApply.length} modifiers")
        } else {
          assert(stateBestBlockId.get sameElements history.bestFullBlockIdOpt.get,
            "State version should always equal to best full block id.")
          log.info(s"State and History are consistent on startup.")
        }
        toApply.foldLeft(state) { (s, m) =>
          s.applyModifier(m) match {
            case Success(newState) =>
              newState
            case Failure(e) =>
              throw new Error(s"Failed to apply missed modifier ${m.encodedId}")
          }
        }
      }.recoverWith { case e =>
        log.error("Failed to recover state, try to resync from genesis manually", e)
        EncryApp.forceStopApplication(500)
      }.get
    }
  }
}

private[view] class DigestEncryNodeViewHolder(settings: EncryAppSettings, timeProvider: NetworkTimeProvider)
  extends EncryNodeViewHolder[DigestState](settings, timeProvider)

private[view] class UtxoEncryNodeViewHolder(settings: EncryAppSettings, timeProvider: NetworkTimeProvider)
  extends EncryNodeViewHolder[UtxoState](settings, timeProvider)

object EncryNodeViewHolder {
  def createActor(system: ActorSystem, settings: EncryAppSettings, timeProvider: NetworkTimeProvider): ActorRef = {
    if (settings.nodeSettings.ADState) system.actorOf(Props.create(classOf[DigestEncryNodeViewHolder], settings, timeProvider))
    else system.actorOf(Props.create(classOf[UtxoEncryNodeViewHolder], settings, timeProvider))
  }
}
