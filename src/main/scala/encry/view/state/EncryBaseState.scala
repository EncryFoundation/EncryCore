package encry.view.state

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.EncryBoxStateChanges
import scorex.core.{EphemerealNodeViewModifier, VersionTag}
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest

import scala.util.Try

trait EncryBaseState[IState <: MinimalState[EncryPersistentModifier, IState]]
  extends MinimalState[EncryPersistentModifier, IState] with ScorexLogging {

  self: IState =>

  def rootHash(): ADDigest

  // TODO: Implement correctly.
  def stateHeight(): Int = 0

  // TODO: Which instance of proposition should be passed here??
  def boxChanges(txs: Seq[EncryBaseTransaction]): EncryBoxStateChanges

  // ID of last applied modifier.
  override def version: VersionTag

  override def applyModifier(mod: EncryPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[VersionTag]

  override type NVCT = this.type

}

object EncryBaseState extends ScorexLogging
