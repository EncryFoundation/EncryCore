package encry

import java.net.InetAddress
import akka.actor.SupervisorStrategy.Restart
import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.server.ExceptionHandler
import akka.stream.ActorMaterializer
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.{ApiRoute, CompositeHttpService, DataHolderForApi}
import encry.api.http.routes._
import encry.cli.ConsoleListener
import encry.cli.ConsoleListener.StartListening
import encry.local.miner.Miner
import encry.local.miner.Miner.StartMining
import encry.network._
import encry.settings.EncryAppSettings
import encry.stats.{StatsSender, Zombie}
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewHolder
import encry.view.mempool.MemoryPool
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import kamon.system.SystemMetrics
import org.encryfoundation.common.utils.Algos
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

object EncryApp extends App with StrictLogging {

  implicit val system: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContextExecutor = system.dispatcher
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  lazy val settings: EncryAppSettings = EncryAppSettings.read(args.headOption)
  val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

  val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")
  val nodeId: Array[Byte] = Algos.hash(settings.network.nodeName
    .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)).take(5)

  val influxRef: Option[ActorRef] =
    settings.influxDB.map(s => system.actorOf(StatsSender.props(s), "statsSender"))

  lazy val dataHolderForApi = system.actorOf(DataHolderForApi.props(settings, timeProvider), "dataHolder")

  lazy val miner: ActorRef = system.actorOf(Miner.props(dataHolderForApi, influxRef), "miner")
  lazy val memoryPool: ActorRef = system.actorOf(MemoryPool.props(settings, timeProvider, miner, influxRef)
    .withDispatcher("mempool-dispatcher"))
   val nodeViewHolder: ActorRef = system.actorOf(NodeViewHolder.props(memoryPool, influxRef, dataHolderForApi)
     .withDispatcher("nvh-dispatcher"), "nodeViewHolder")

  val nodeViewSynchronizer: ActorRef = system.actorOf(NodeViewSynchronizer
    .props(influxRef, nodeViewHolder, settings, memoryPool, dataHolderForApi)
    .withDispatcher("nvsh-dispatcher"), "nodeViewSynchronizer")

  if (settings.monitoringSettings.exists(_.kamonEnabled)) {
    Kamon.reconfigure(EncryAppSettings.allConfig)
    Kamon.addReporter(new InfluxDBReporter())
    SystemMetrics.startCollecting()
  }
  if (settings.node.mining) miner ! StartMining
  if (settings.node.useCli) {
    system.actorOf(Props[ConsoleListener], "cliListener")
    system.actorSelection("/user/cliListener") ! StartListening
  }

  if (settings.restApi.enabled.getOrElse(false)) {
    import akka.http.scaladsl.model.StatusCodes._
    import akka.http.scaladsl.server.Directives._

    implicit def apiExceptionHandler: ExceptionHandler = ExceptionHandler {
      case e: Exception => extractUri { uri =>
        logger.info(s"Request to $uri could not be handled normally due to: $e")
        complete(HttpResponse(InternalServerError, entity = "Internal server error"))
      }
    }

    val apiRoutes: Seq[ApiRoute] = Seq(
      PeersApiRoute(settings.restApi, dataHolderForApi),
      InfoApiRoute(dataHolderForApi, settings, nodeId, timeProvider),
      HistoryApiRoute(dataHolderForApi, settings, nodeId),
      TransactionsApiRoute(dataHolderForApi, memoryPool,  settings.restApi),
      WalletInfoApiRoute(dataHolderForApi, settings.restApi)
    )
    Http().bindAndHandle(
      CompositeHttpService(system, apiRoutes, settings.restApi, swaggerConfig).compositeRoute,
      settings.restApi.bindAddress.getAddress.getHostAddress,
      settings.restApi.bindAddress.getPort)
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