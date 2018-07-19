package encry.utils

import encry.EncryApp._
import akka.actor.{Actor, DeadLetter}

class Zombie extends Actor with Logging {

  override def preStart(): Unit = {
    system.eventStream.subscribe(self, classOf[DeadLetter])
  }

  override def receive: Receive = {
    case deadMessage: DeadLetter => logger.debug(s"Dead letter: ${deadMessage.toString}.")
  }

}
