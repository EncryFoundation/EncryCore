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
  extends EncryBaseNodeViewHolder[EncryPersistentModifier] {

  // TODO: `settings.scorexSettings.network.networkChunkSize` should be used.
  override val networkChunkSize: Int = 400

  override type MS = StateType
  override type SI = EncrySyncInfo
  override type HIS = EncryHistory
  override type VL = EncryWallet
  override type MP = EncryMempool

  // TODO: What about transactions of different types?
  override lazy val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] = Map(
    EncryBlockHeader.modifierTypeId     -> EncryBlockHeaderSerializer,
    EncryBlockPayload.modifierTypeId    -> EncryBlockPayloadSerializer,
    ADProofs.modifierTypeId             -> ADProofSerializer,
    EncryBaseTransaction.ModifierTypeId -> EncryPaymentTransactionSerializer
  )
}
