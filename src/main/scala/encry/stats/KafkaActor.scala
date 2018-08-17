package encry.stats

import akka.actor.Actor
import encry.utils.Logging

class KafkaActor extends Actor with Logging {

  import KafkaActor.KafkaMessage

  override def receive: Receive = {
    case KafkaMessage(msg: String) => logger.info(msg)
  }

  override def preStart(): Unit = super.preStart()

  override def postStop(): Unit = super.postStop()

}

object KafkaActor {

  case class KafkaMessage(msg: String)

}