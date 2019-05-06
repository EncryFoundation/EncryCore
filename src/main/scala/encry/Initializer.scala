package encry

import akka.actor.{Actor, ActorRef, Props}
import encry.settings.EncryAppSettings
import encry.stats.StatsSender
import encry.utils.NetworkTimeProvider

class Initializer(settings: EncryAppSettings,
                  timeProvider: NetworkTimeProvider) extends Actor {

  val influxRef: Option[ActorRef] = settings.influxDB.map(_ => context.actorOf(Props[StatsSender]))


  override def receive: Receive = {
    case _ =>
  }
}

object Initializer {

  def props(settings: EncryAppSettings, timeProvider: NetworkTimeProvider): Props =
    Props(new Initializer(settings, timeProvider))
}