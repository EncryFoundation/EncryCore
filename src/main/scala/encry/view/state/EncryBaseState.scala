package encry.view.state

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.{EncryBaseTransaction, EncryPaymentTransaction}
import encry.modifiers.state.box.{EncryBaseBox, EncryBoxStateChanges, Insertion, Removal}
import encry.modifiers.state.box.body.BaseBoxBody
import scorex.core.{EphemerealNodeViewModifier, VersionTag}
import scorex.core.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.authds.ADKey

import scala.util.Try

trait EncryBaseState[P <: Proposition, BB <: BaseBoxBody, BX <: EncryBaseBox[P, BB],
  TX <: EncryBaseTransaction[P, BB, BX], IState <: MinimalState[EncryPersistentModifier, IState]]
  extends MinimalState[EncryPersistentModifier, IState] with ScorexLogging {

  self: IState =>

  def rootHash(): ADDigest

  // TODO: Implement correctly.
  def stateHeight(): Int = 0

  // TODO: Which instance of proposition should be passed here??
  def boxChanges(txs: Seq[EphemerealNodeViewModifier]): EncryBoxStateChanges

  // ID of last applied modifier.
  override def version: VersionTag

  override def applyModifier(mod: EncryPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[VersionTag]

  override type NVCT = this.type

}

object EncryBaseState extends ScorexLogging
