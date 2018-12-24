package encry.storage.levelDb.forksTree

import encry.modifiers.state.box.EncryBaseBox
import org.encryfoundation.common.utils.TaggedTypes.ADKey

trait Diff {
  def revert: Diff
}

case class WalletDiff(boxesToRemove: Seq[ADKey],
                      boxesToAdd: Seq[EncryBaseBox],
                      balanceChanges: Map[Array[Byte], Long]) extends Diff {
  override def revert: Diff =
    this.copy(balanceChanges = this.balanceChanges.map(assetBalance => assetBalance.copy(_2 = assetBalance._2 * -1)))
}