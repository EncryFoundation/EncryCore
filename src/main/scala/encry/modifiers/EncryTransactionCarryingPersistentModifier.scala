package encry.modifiers

import encry.modifiers.mempool.EncryBaseTransaction

trait EncryTransactionCarryingPersistentModifier extends EncryPersistentModifier {

  def transactions: Seq[EncryBaseTransaction]
}
