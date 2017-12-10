package encry.modifiers.history

import encry.modifiers.EncryPersistentModifier
import scorex.core.ModifierId
import scorex.core.block.Block._
import scorex.crypto.hash.Digest32

trait EncryBaseBlockHeader extends EncryPersistentModifier {
  val version: Version
  override def parentId: ModifierId
  val txMerkleRoot: Digest32
  val timestamp: Timestamp
  val height: Int
}
