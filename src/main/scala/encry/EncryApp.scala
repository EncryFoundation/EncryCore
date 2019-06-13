package encry

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi
import encry.cli.ConsoleListener
import encry.cli.ConsoleListener.StartListening
import encry.local.miner.Miner
import encry.local.miner.Miner.StartMining
import encry.network._
import encry.settings.EncryAppSettings
import encry.stats.{KafkaActor, StatsSender, Zombie}
import encry.utils.NetworkTimeProvider
import encry.view.mempool.Mempool
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

  lazy val settings: EncryAppSettings = EncryAppSettings.read(args.headOption)
  lazy val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

  val influxRef: Option[ActorRef] =
    if (settings.influxDB.isDefined) Some(system.actorOf(Props[StatsSender], "statsSender"))
    else None

  lazy val dataHolderForApi = system.actorOf(DataHolderForApi.props(settings, timeProvider), "dataHolder")

  lazy val miner: ActorRef = system.actorOf(Miner.props(dataHolderForApi, influxRef), "miner")
  lazy val memoryPool: ActorRef = system.actorOf(Mempool.props(settings, timeProvider, miner)
    .withDispatcher("mempool-dispatcher"))
  lazy val nodeViewHolder: ActorRef = system.actorOf(NodeViewHolder.props(memoryPool, influxRef, dataHolderForApi)
    .withMailbox("nvh-mailbox"), "nodeViewHolder")

  lazy val nodeViewSynchronizer: ActorRef = system.actorOf(NodeViewSynchronizer
    .props(influxRef, nodeViewHolder, settings, memoryPool, dataHolderForApi)
    .withDispatcher("nvsh-dispatcher"), "nodeViewSynchronizer")

  if (settings.monitoringSettings.exists(_.kamonEnabled)) {
    Kamon.reconfigure(EncryAppSettings.allConfig)
    Kamon.addReporter(new InfluxDBReporter())
    SystemMetrics.startCollecting()
  }
  if (settings.kafka.exists(_.sendToKafka))
    system.actorOf(Props[KafkaActor].withDispatcher("kafka-dispatcher"), "kafkaActor")
  if (settings.node.mining) miner ! StartMining
  if (settings.node.useCli) {
    system.actorOf(Props[ConsoleListener], "cliListener")
    system.actorSelection("/user/cliListener") ! StartListening
  }

  system.actorOf(Props[Zombie], "zombie")

  def forceStopApplication(code: Int = 0): Nothing = {
    system.registerOnTermination {
      println("Actor system is terminated")
    }
    Await.ready(system.terminate(), 1 minute)
    sys.exit(code)
  }

  def commonSupervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = 5,
    withinTimeRange = 60 seconds) {
    case _ => Restart
  }
}