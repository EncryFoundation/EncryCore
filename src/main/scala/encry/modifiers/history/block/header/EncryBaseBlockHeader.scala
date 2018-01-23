package encry.modifiers.history.block.header

import encry.modifiers.{EncryPersistentModifier, Signable25519}
import scorex.core.ModifierId
import scorex.core.block.Block._
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

trait EncryBaseBlockHeader extends EncryPersistentModifier with Signable25519 {

  val version: Version

  override def parentId: ModifierId

  val adProofsRoot: Digest32

  val stateRoot: ADDigest

  val txsRoot: Digest32

  val timestamp: Timestamp

  val height: Int
}
