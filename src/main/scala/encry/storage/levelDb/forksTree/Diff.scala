package encry.storage.levelDb.forksTree

import encry.modifiers.state.box.{EncryBox, EncryProposition}

trait Diff {
  def revert: Diff
}

case class WalletDiff(boxesToRemove: Seq[EncryBox[EncryProposition]],
                      boxesToAdd: Seq[EncryBox[EncryProposition]],
                      balanceChanges: Map[Array[Byte], Long]) extends Diff {
  override def revert: Diff =
    this.copy(balanceChanges = this.balanceChanges.map(assetBalance => assetBalance.copy(_2 = assetBalance._2 * -1)))
}