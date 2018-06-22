package encry.modifiers

import encry.settings.Algos
import encry.{ModifierId, ModifierTypeId}

trait ModifierWithDigest extends PersistentNodeViewModifier {

  override lazy val id: ModifierId = ModifierWithDigest.computeId(modifierTypeId, headerId, digest)

  // Likely Merkle Root hash
  def digest: Array[Byte]
  def headerId: Array[Byte]
}

object ModifierWithDigest {
  def computeId(modifierType: ModifierTypeId, headerId: Array[Byte], digest: Array[Byte]): ModifierId =
    ModifierId @@ Algos.hash.prefixedHash(modifierType, headerId, digest)
}
