package encry.network

import akka.persistence.{PersistentActor, SnapshotOffer}
import encry.network.ModifiersHolder.{NewBlock, State}
import encry.utils.ScorexLogging

class ModifiersHolder extends PersistentActor with ScorexLogging {

  var counter: State = State(0)

  override def preStart(): Unit = {
    logger.info(s"Before start counter: ${counter.counter}")
  }

  override def receiveRecover: Receive = {
    case NewBlock => updateCounter()
    case SnapshotOffer(_, snapshot: Int) => counter = State(snapshot)
  }

  override def receiveCommand: Receive = {
    case NewBlock =>
      logger.info(s"New block is here. Before incrementing: ${counter.counter}")
      persist(counter) { _ => updateCounter() }
    case x: Any => println(s"+++ $x")
  }

  override def persistenceId: String = "persistent actor"

  override def journalPluginId: String = "akka.persistence.journal.leveldb"

  override def snapshotPluginId: String = "akka.persistence.snapshot-store.local"

  def updateCounter(): Unit = {
    counter = State(counter.counter + 1)
  }
}

object ModifiersHolder {

  case object NewBlock

  case class State(counter: Int)

}
