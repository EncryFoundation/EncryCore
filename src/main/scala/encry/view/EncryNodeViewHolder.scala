package encry.view

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.{ADProofSerializer, ADProofs}
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.history.block.payload.{EncryBlockPayload, EncryBlockPayloadSerializer}
import encry.modifiers.mempool.{EncryBaseTransaction, EncryPaymentTransactionSerializer}
import encry.settings.EncryAppSettings
import encry.view.history.{EncryHistory, EncrySyncInfo}
import encry.view.mempool.EncryMempool
import encry.view.state.EncryState
import encry.view.wallet.EncryWallet
import scorex.core.serialization.Serializer
import scorex.core.{ModifierTypeId, NodeViewHolder, NodeViewModifier}
import scorex.core.transaction.box.proposition.Proposition

abstract class EncryNodeViewHolder[StateType <: EncryState[StateType]](settings: EncryAppSettings)
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
    EncryBaseTransaction.ModifierTypeId -> EncryPaymentTransactionSerializer
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
    val dir = EncryState.stateDir(settings)
    dir.mkdir()
    assert(dir.listFiles().isEmpty, s"Genesis directory $dir should always be empty")

    val state = (
      if (settings.nodeSettings.ADState) EncryState.generateGenesisDigestState(dir, settings.nodeSettings)
      else EncryState.generateGenesisUtxoState(dir, Some(self))._1
      ).asInstanceOf[MS]

    //todo: ensure that history is in certain mode
    val history = EncryHistory.readOrGenerate(settings)

    val wallet = ErgoWallet.readOrGenerate(settings)

    val memPool = ErgoMemPool.empty

    (history, state, wallet, memPool)
  }
}
