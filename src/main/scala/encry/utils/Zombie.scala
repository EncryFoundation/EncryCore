package encry.utils

import akka.actor.{Actor, DeadLetter, UnhandledMessage}
import encry.stats.LoggingActor.LogMessage
import encry.EncryApp.{settings, system}

class Zombie extends Actor {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[DeadLetter])
    context.system.eventStream.subscribe(self, classOf[UnhandledMessage])
  }

  override def receive: Receive = {
    case deadMessage: DeadLetter => if (settings.logging.enableLogging) system.actorSelection("user/loggingActor") !
      LogMessage("Debug", s"Dead letter: ${deadMessage.toString}.", System.currentTimeMillis())
    case unhandled: UnhandledMessage => if (settings.logging.enableLogging) system.actorSelection("user/loggingActor") !
      LogMessage("Debug", s"Unhandled letter: ${unhandled.toString}.", System.currentTimeMillis())
  }

}
