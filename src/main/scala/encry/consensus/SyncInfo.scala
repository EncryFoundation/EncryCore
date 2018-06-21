package encry.consensus

import encry.modifiers.BytesSerializable

/**
  * Syncing info provides information about starting points this node recommends another to start
  * synchronization from
  */
trait SyncInfo extends BytesSerializable {
  def startingPoints: History.ModifierIds
}


