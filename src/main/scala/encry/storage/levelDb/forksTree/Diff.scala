package encry.storage.levelDb.forksTree

import encry.modifiers.state.StateModifierSerializer
import encry.modifiers.state.box.EncryBaseBox
import org.encryfoundation.common.Algos.HF
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import scorex.crypto.hash.Digest32
import cats.syntax.semigroup._
import cats.instances.all._
import com.typesafe.scalalogging.StrictLogging
import org.encryfoundation.common.Algos

import scala.util.Success

trait Diff

trait RevertabaleDiff[D <: Diff] extends Diff {

  def ++(diff: D): D

  def revert(persistantProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF]): D
}

case class WalletDiff(boxesToRemove: Seq[ADKey],
                      boxesToAdd: Seq[EncryBaseBox],
                      balanceChanges: Map[String, Long]) extends RevertabaleDiff[WalletDiff] with StrictLogging{
  override def revert(persistantProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF]): WalletDiff =
    this.copy(
      boxesToAdd = this.boxesToRemove.flatMap(boxId => persistantProver.unauthenticatedLookup(boxId)
          .map(bytes => StateModifierSerializer.parseBytes(bytes, boxId.head))).collect{case Success(x) => x},
      boxesToRemove = this.boxesToAdd.map(_.id),
      balanceChanges = this.balanceChanges.map(assetBalance => assetBalance.copy(_2 = assetBalance._2 * -1))
    )

  override def ++(diff: WalletDiff): WalletDiff = {
    WalletDiff(
      this.boxesToRemove ++ diff.boxesToRemove,
      this.boxesToAdd ++ diff.boxesToAdd,
      this.balanceChanges |+| diff.balanceChanges
    )
  }
}