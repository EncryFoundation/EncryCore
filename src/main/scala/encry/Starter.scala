package encry

import java.net.InetAddress
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.server.ExceptionHandler
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.{ec, materializer}
import encry.Starter.{AuxHistoryIsReady, NVHHistoryIsReady}
import encry.api.http.{ApiRoute, CompositeHttpService, PeersApiRoute, UtilsApiRoute}
import encry.api.http.routes._
import encry.cli.ConsoleListener
import encry.local.explorer.BlockListener
import encry.local.explorer.database.DBService
import encry.local.miner.Miner
import encry.local.miner.Miner.StartMining
import encry.network.{AuxiliaryHistoryHolder, NetworkController, NodeViewSynchronizer, PeerManager}
import encry.settings.EncryAppSettings
import encry.stats.{KafkaActor, StatsSender, Zombie}
import encry.utils.NetworkTimeProvider
import encry.view.{EncryNodeViewHolder, ReadersHolder}
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import kamon.system.SystemMetrics
import org.encryfoundation.common.Algos
import scala.concurrent.Future
import scala.io.Source

class Starter(actorSystem: ActorSystem,
              appSettings: EncryAppSettings,
              timeProvider: NetworkTimeProvider,
              dbService: DBService) extends Actor with StrictLogging {

  var nvhIsReady: Boolean = false
  var auxIsReady: Boolean = false

  val influxRef: Option[ActorRef] =
    appSettings.influxDB.map(_ => actorSystem.actorOf(Props[StatsSender], "statsSender"))

  val auxHistoryHolder: ActorRef = actorSystem.actorOf(Props(AuxiliaryHistoryHolder(appSettings, timeProvider, self))
    .withDispatcher("aux-history-dispatcher"), "auxHistoryHolder")

  val nodeViewHolder: ActorRef = actorSystem.actorOf(EncryNodeViewHolder.props(auxHistoryHolder, self, influxRef)
    .withDispatcher("nvh-dispatcher"), "nodeViewHolder")

  override def receive: Receive = {
    case NVHHistoryIsReady =>
      nvhIsReady = true
      checkForInitialisation()
    case AuxHistoryIsReady =>
      auxIsReady = true
      checkForInitialisation()
    case message => logger.info(s"Got message $message on starter actor during iodbs initialising.")
  }

  def behaviourAfterHistoryInitialisations: Receive = {
    case message => logger.info(s"Got message $message on starter actor!")
  }

  def checkForInitialisation(): Unit = if (nvhIsReady && auxIsReady) {
    startFullAppFunctions()
    context.become(behaviourAfterHistoryInitialisations)
    logger.info(s"All iodb storages are initialized. Starting main app functions.")
  }

  def startFullAppFunctions(): Unit = {

    val miner: ActorRef = actorSystem.actorOf(Props(classOf[Miner], nodeViewHolder), "miner")
    if (appSettings.node.mining) miner ! StartMining

    val networkController: ActorRef = actorSystem.actorOf(Props[NetworkController]
      .withDispatcher("network-dispatcher"), "networkController")

    val nodeViewSync: ActorRef = actorSystem.actorOf(Props(classOf[NodeViewSynchronizer],
      influxRef, nodeViewHolder, networkController, auxHistoryHolder, actorSystem, appSettings), "nodeViewSynchronizer")

    val peerManager: ActorRef =
      actorSystem.actorOf(Props(classOf[PeerManager], nodeViewSync, networkController), "peerManager")

    val readersHolder: ActorRef = actorSystem.actorOf(Props(classOf[ReadersHolder]), "readersHolder")

    if (appSettings.postgres.exists(_.enableSave)) actorSystem.actorOf(Props(classOf[BlockListener],
      dbService, nodeViewHolder).withDispatcher("block-listener-dispatcher"), "blockListener")

    if (appSettings.node.useCli) actorSystem.actorOf(Props[ConsoleListener], "cliListener")

    actorSystem.actorOf(Props[Zombie], "zombie")

    if (appSettings.restApi.enabled.getOrElse(false)) {
      import akka.http.scaladsl.model.StatusCodes._
      import akka.http.scaladsl.server.Directives._
      implicit def apiExceptionHandler: ExceptionHandler =
        ExceptionHandler {
          case e: Exception =>
            extractUri { uri =>
              logger.info(s"Request to $uri could not be handled normally due to: $e")
              complete(HttpResponse(InternalServerError, entity = "Internal server error"))
            }
        }

      val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")
      val nodeId: Array[Byte] = Algos.hash(appSettings.network.nodeName.getOrElse(
        InetAddress.getLocalHost.getHostAddress + ":" + appSettings.network.bindAddress.getPort)
      ).take(5)

      val apiRoutes: Seq[ApiRoute] = Seq(
        UtilsApiRoute(appSettings.restApi),
        PeersApiRoute(peerManager, networkController, appSettings.restApi),
        InfoApiRoute(readersHolder, miner, peerManager, appSettings, nodeId, timeProvider),
        HistoryApiRoute(readersHolder, miner, appSettings, nodeId, appSettings.node.stateMode),
        TransactionsApiRoute(readersHolder, nodeViewHolder, appSettings.restApi, appSettings.node.stateMode),
        StateInfoApiRoute(readersHolder, nodeViewHolder, appSettings.restApi, appSettings.node.stateMode),
        WalletInfoApiRoute(nodeViewHolder, appSettings.restApi)
      )
      setupHttpService(appSettings, actorSystem, apiRoutes, swaggerConfig)
    }

    if (appSettings.monitoringSettings.exists(_.kamonEnabled)) {
      Kamon.reconfigure(EncryAppSettings.allConfig)
      Kamon.addReporter(new InfluxDBReporter())
      SystemMetrics.startCollecting()
    }

    if (appSettings.kafka.exists(_.sendToKafka))
      actorSystem.actorOf(Props[KafkaActor].withDispatcher("kafka-dispatcher"), "kafkaActor")
  }

  def setupHttpService(appSettings: EncryAppSettings,
                       actorSystem: ActorSystem,
                       apiRoutes: Seq[ApiRoute],
                       swaggerConfig: String): Future[Http.ServerBinding] = Http(actorSystem).bindAndHandle(
    CompositeHttpService(actorSystem, apiRoutes, appSettings.restApi, swaggerConfig).compositeRoute,
    appSettings.restApi.bindAddress.getAddress.getHostAddress,
    appSettings.restApi.bindAddress.getPort
  )
}

object Starter {

  case object NVHHistoryIsReady

  case object AuxHistoryIsReady

  case object MessageWithNodeViewSyncActorRef

  case object MessageWithMinerRef

  case object MessageWithPeerHandlerRef

  case object NVHActorIsReady
}