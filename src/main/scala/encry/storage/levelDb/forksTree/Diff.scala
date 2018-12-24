package encry.storage.levelDb.forksTree

import encry.modifiers.state.StateModifierSerializer
import encry.modifiers.state.box.EncryBaseBox
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.iq80.leveldb.DB

trait Diff {
  def revert(db: DB): Diff
}

case class WalletDiff(boxesToRemove: Seq[ADKey],
                      boxesToAdd: Seq[EncryBaseBox],
                      balanceChanges: Map[Array[Byte], Long]) extends Diff {
  override def revert(db: DB): Diff =
    this.copy(
      boxesToAdd = this.boxesToRemove.map(boxId => StateModifierSerializer.parseBytes(db.get(boxId), boxId.head).get),
      boxesToRemove = this.boxesToAdd.map(_.id),
      balanceChanges = this.balanceChanges.map(assetBalance => assetBalance.copy(_2 = assetBalance._2 * -1))
    )
}