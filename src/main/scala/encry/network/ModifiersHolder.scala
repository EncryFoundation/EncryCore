package encry.network

import encry.EncryApp._
import akka.persistence.{PersistentActor, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
import encry.ModifierTypeId
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.{EncryPersistentModifier, NodeViewModifier}
import encry.network.ModifiersHolder.{Amount, Mods, RequestedModifiers}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.utils.Logging
import encry.view.EncryNodeViewHolder.ReceivableMessages.{LocallyGeneratedModifier, ModifiersFromRemote}
import scala.concurrent.duration._

class ModifiersHolder extends PersistentActor with Logging {

  var headers: Seq[EncryBlockHeader] = Seq.empty
  var payloads: Seq[EncryBlockPayload] = Seq.empty
  var blocks: Seq[EncryBlock] = Seq.empty
  var modsFromRemote: Mods = Mods(Map.empty, 0)
  var amount: Amount = Amount(0, 0, 0)

  context.system.scheduler.schedule(10.second, 10.second) {
    amount = Amount(headers.size, payloads.size, blocks.size)
    saveSnapshot(amount)
    logger.info(s"ModifiersHolder: ${amount.headers} - ${amount.payloads} - ${amount.blocks}")
  }

  override def preStart(): Unit = logger.info(s"ModifiersHolder actor is started.")

  override def receiveRecover: Receive = {
    case mods: ModifiersFromRemote => updateModsFromRemote(mods: ModifiersFromRemote)
    case SnapshotOffer(_, snapshot: Mods) => modsFromRemote = snapshot
    case SnapshotOffer(_, snapshot: Amount) => amount = snapshot
  }

  override def receiveCommand: Receive = {
    case SaveSnapshotSuccess(metadata) => logger.info("Success with snapshot")
    case SaveSnapshotFailure(metadata, reason) => logger.info("Failure with snapshot")
    case RequestedModifiers(modifierTypeId, modifiers) => updateModifiers(modifierTypeId, modifiers)
    case lm: LocallyGeneratedModifier[EncryPersistentModifier] => updateModifiers(lm.pmod.modifierTypeId, Seq(lm.pmod))
    case x: Any => logger.info(s"Strange input: $x")
  }

  override def persistenceId: String = "persistent actor"

  override def journalPluginId: String = "akka.persistence.journal.leveldb"

  override def snapshotPluginId: String = "akka.persistence.snapshot-store.local"

  def updateModsFromRemote(newMods: ModifiersFromRemote): Unit = _

  def updateModifiers(modsTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier]): Unit = modifiers.foreach {
    case header: EncryBlockHeader => headers = headers :+ header
    case payload: EncryBlockPayload => payloads = payloads :+ payload
    case block: EncryBlock => blocks = blocks :+ block
    case _ =>
  }
}

object ModifiersHolder {

  case class Amount(headers: Int, payloads: Int, blocks: Int)

  case class RequestedModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[NodeViewModifier])

  case class Mods(numberOfModsByPeerAndModType: Map[(ConnectedPeer, ModifierTypeId), Int], numberOfPacksFromRemotes: Int)

}