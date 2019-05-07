package encry

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.network._
import encry.settings.EncryAppSettings
import encry.stats.{StatsSender, Zombie}
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewHolder
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import kamon.system.SystemMetrics
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.concurrent.duration._
import scala.language.postfixOps

object EncryApp extends App with StrictLogging {

  implicit val system: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  val settings: EncryAppSettings = EncryAppSettings.read(args.headOption)
  val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

  val influxRef: Option[ActorRef] = if (settings.influxDB.isDefined)
    Some(system.actorOf(StatsSender.props(settings, timeProvider)))
  else None

  system.actorOf(Props[Zombie])

  val auxHistoryHolder: ActorRef =
    system.actorOf(AuxiliaryHistoryHolder.props(settings, timeProvider).withDispatcher("aux-history-dispatcher"))

  val nodeViewHolder: ActorRef =
    system.actorOf(NodeViewHolder.props(auxHistoryHolder, settings, influxRef, timeProvider)
      .withDispatcher("nvh-dispatcher"))

  if (settings.monitoringSettings.exists(_.kamonEnabled)) {
    Kamon.reconfigure(EncryAppSettings.allConfig)
    Kamon.addReporter(new InfluxDBReporter())
    SystemMetrics.startCollecting()
  }

  def forceStopApplication(code: Int = 0): Nothing = {
    system.registerOnTermination(println("Actor system is terminated"))
    Await.ready(system.terminate(), 1 minute)
    sys.exit(code)
  }

  def commonSupervisorStrategy: OneForOneStrategy =
    OneForOneStrategy(maxNrOfRetries = 5, withinTimeRange = 60 seconds) {
      case _ => Restart
    }
}