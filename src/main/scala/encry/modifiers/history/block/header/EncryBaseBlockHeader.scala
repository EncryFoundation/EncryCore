package encry.modifiers.history.block.header

import encry.modifiers.EncryPersistentModifier
import encry.ModifierId
import encry.modifiers.history.block.Block._
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

trait EncryBaseBlockHeader extends EncryPersistentModifier {

  val version: Version

  override def parentId: ModifierId

  val adProofsRoot: Digest32

  val stateRoot: ADDigest

  val transactionsRoot: Digest32

  val timestamp: Timestamp

  val height: Int
}
