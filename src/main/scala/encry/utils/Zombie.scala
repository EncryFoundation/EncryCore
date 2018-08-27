package encry.utils

import akka.actor.{Actor, DeadLetter, UnhandledMessage}

class Zombie extends Actor with Logging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[DeadLetter])
    context.system.eventStream.subscribe(self, classOf[UnhandledMessage])
  }

  override def receive: Receive = {
    case deadMessage: DeadLetter => debug(s"Dead letter: ${deadMessage.toString}.")
    case unhandled: UnhandledMessage => debug(s"Unhandled letter: ${unhandled.toString}.")
  }

}
