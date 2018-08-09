package encry.consensus

import org.encryfoundation.common.serialization.BytesSerializable

/** Syncing info provides information about starting points this node recommends another to start
  * synchronization from */
trait SyncInfo extends BytesSerializable {
  def startingPoints: History.ModifierIds
}
