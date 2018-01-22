package encry.local

import akka.actor.ActorRef
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.EncryBaseTransaction
import encry.settings.EncryAppSettings
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{LocalInterface, ModifierId}

class EncryLocalInterface (override val viewHolderRef: ActorRef, miner: ActorRef, settings: EncryAppSettings)
  extends LocalInterface[Proposition, EncryBaseTransaction, EncryPersistentModifier] {

  override protected def onStartingPersistentModifierApplication(pmod: EncryPersistentModifier): Unit = {}

  override protected def onFailedTransaction(tx: EncryBaseTransaction): Unit = {}

  override protected def onSuccessfulTransaction(tx: EncryBaseTransaction): Unit = {}

  override protected def onNoBetterNeighbour(): Unit = {}

  override protected def onBetterNeighbourAppeared(): Unit = {}

  override protected def onSyntacticallySuccessfulModification(mod: EncryPersistentModifier): Unit = {}

  override protected def onSyntacticallyFailedModification(mod: EncryPersistentModifier): Unit = {}

  override protected def onSemanticallySuccessfulModification(mod: EncryPersistentModifier): Unit = {}

  override protected def onSemanticallyFailedModification(mod: EncryPersistentModifier): Unit = {}

  override protected def onNewSurface(newSurface: Seq[ModifierId]): Unit = {}

  override protected def onRollbackFailed(): Unit = {}
}
