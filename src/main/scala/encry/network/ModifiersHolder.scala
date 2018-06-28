package encry.network

import akka.persistence.{PersistentActor, SnapshotOffer}
import encry.ModifierTypeId
import encry.network.ModifiersHolder.Mods
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.utils.ScorexLogging
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote

class ModifiersHolder extends PersistentActor with ScorexLogging {

  var modsFromRemote: Mods = Mods(Map.empty, 0)

  override def preStart(): Unit = logger.info(s"Started fucking actor")

  override def receiveRecover: Receive = {
    case mods: ModifiersFromRemote => updateModsFromRemote(mods: ModifiersFromRemote)
    case SnapshotOffer(_, snapshot: Mods) => modsFromRemote = snapshot
  }

  override def receiveCommand: Receive = {
    case mods@ModifiersFromRemote(peer, modifierTypeId, remoteObjects) =>
      logger.info(s"Already received ${modsFromRemote.numberOfPacksFromRemotes} packs of modifiers. \n$modsFromRemote")
      logger.info(s"New pack of modifiers $modifierTypeId from $peer. Size: ${remoteObjects.size}.")
      persist(mods) { _ => updateModsFromRemote(mods) }
    case x: Any => logger.info(s"Strange input: $x")
  }

  override def persistenceId: String = "persistent actor"

  override def journalPluginId: String = "akka.persistence.journal.leveldb"

  override def snapshotPluginId: String = "akka.persistence.snapshot-store.local"

  def updateModsFromRemote(newMods: ModifiersFromRemote): Unit = {
    modsFromRemote = Mods(modsFromRemote.numberOfModsByPeerAndModType + ((newMods.source, newMods.modifierTypeId) -> newMods.remoteObjects.size), modsFromRemote.numberOfPacksFromRemotes + 1)
  }
}

object ModifiersHolder {

  case class Mods(numberOfModsByPeerAndModType: Map[(ConnectedPeer, ModifierTypeId), Int], numberOfPacksFromRemotes: Int)

}