package encry.modifiers.history.block

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBaseBlockPayload
import scorex.core.EphemerealNodeViewModifier

trait EncryBaseBlock extends EncryPersistentModifier {

  val header: EncryBlockHeader

  val payload: EncryBaseBlockPayload

  val toSeq: Seq[EncryPersistentModifier]

  def transactions: Seq[EphemerealNodeViewModifier]
}

