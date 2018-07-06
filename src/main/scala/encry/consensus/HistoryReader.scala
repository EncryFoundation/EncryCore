package encry.consensus

import encry.ModifierId
import encry.modifiers.PersistentNodeViewModifier
import encry.view.wallet.NodeViewComponent

trait HistoryReader[PM <: PersistentNodeViewModifier, SI <: SyncInfo] extends NodeViewComponent {

  import History._

  def contains(persistentModifier: PM): Boolean = contains(persistentModifier.id)

  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

  def modifierById(modifierId: ModifierId): Option[PM]

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  def compare(other: SI): HistoryComparisonResult
}
