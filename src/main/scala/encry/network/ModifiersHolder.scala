package encry.network

import akka.persistence.{PersistentActor, SnapshotOffer}
import encry.network.ModifiersHolder.NewMessageToPersistentActor
import encry.utils.ScorexLogging

class ModifiersHolder extends PersistentActor with ScorexLogging {

  var counter: Int = 0

  override def receiveRecover: Receive = {
    case NewMessageToPersistentActor =>
      logger.info("------------- Message received on recovery ")
      updateCounter()
    case SnapshotOffer(_, snapshot: Int) =>
      logger.info("------------- Snapshot received on recovery ")
      counter = snapshot
  }

  override def receiveCommand: Receive = {
    case cmd@Int =>
      logger.info("------------- Command received " + cmd)
      persist(Int) { _ => updateCounter() }
    case x: Any => println(x)
  }

  override def persistenceId: String = "persistent actor"

  override def journalPluginId: String = "akka.persistence.journal.leveldb"

  override def snapshotPluginId: String = "akka.persistence.snapshot-store.local"

  def updateCounter(): Unit = {
    counter += 1
    logger.info("------------- Current counter " + counter)
  }
}

object ModifiersHolder {

  case object NewMessageToPersistentActor

}
