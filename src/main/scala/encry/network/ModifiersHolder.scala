package encry.network

import akka.persistence.PersistentActor
import encry.utils.ScorexLogging

class ModifiersHolder extends PersistentActor with ScorexLogging {
  override def receiveRecover: Receive = ???

  override def receiveCommand: Receive = ???

  override def persistenceId: String = ???
}
