package encry.stats

import akka.actor.{Actor, DeadLetter, UnhandledMessage}
import com.typesafe.scalalogging.StrictLogging

class Zombie extends Actor with StrictLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[DeadLetter])
    context.system.eventStream.subscribe(self, classOf[UnhandledMessage])
  }

  override def receive: Receive = {
    case deadMessage: DeadLetter => logger.info(s"Dead letter: ${deadMessage.toString}." +
      s"From: ${deadMessage.sender}. To ${deadMessage.recipient}")
    case unhandled: UnhandledMessage => logger.info(s"Unhandled letter: ${unhandled.toString}. " +
      s"From: ${unhandled.sender}. To ${unhandled.recipient}")
  }
}
