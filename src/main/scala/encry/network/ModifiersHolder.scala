package encry.network

import akka.persistence.{PersistentActor, SnapshotOffer}
import encry.ModifierTypeId
import encry.modifiers.{EncryPersistentModifier, NodeViewModifier}
import encry.network.ModifiersHolder.{Mods, RequestedModifiers}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.utils.ScorexLogging
import encry.view.EncryNodeViewHolder.ReceivableMessages.{LocallyGeneratedModifier, ModifiersFromRemote}

class ModifiersHolder extends PersistentActor with ScorexLogging {

  var modsFromRemote: Mods = Mods(Map.empty, 0)

  override def preStart(): Unit = logger.info(s"Started fucking actor")

  override def receiveRecover: Receive = {
    case mods: ModifiersFromRemote => updateModsFromRemote(mods: ModifiersFromRemote)
    case SnapshotOffer(_, snapshot: Mods) => modsFromRemote = snapshot
  }

  override def receiveCommand: Receive = {
    case RequestedModifiers(modifierTypeId, modifiers) => _
    case lm: LocallyGeneratedModifier[EncryPersistentModifier] => _
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

  case class RequestedModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier])

  case class Mods(numberOfModsByPeerAndModType: Map[(ConnectedPeer, ModifierTypeId), Int], numberOfPacksFromRemotes: Int)

}