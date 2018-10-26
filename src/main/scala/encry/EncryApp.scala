package encry

import java.net.InetAddress
import akka.actor.SupervisorStrategy.Restart
import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.server.ExceptionHandler
import akka.stream.ActorMaterializer
import encry.api.http.routes._
import encry.api.http.{ApiRoute, CompositeHttpService, PeersApiRoute, UtilsApiRoute}
import encry.cli.ConsoleListener
import encry.cli.ConsoleListener.StartListening
import encry.local.explorer.BlockListener
import encry.local.explorer.database.DBService
import encry.local.miner.Miner
import encry.local.miner.Miner.StartMining
import encry.network.message._
import encry.network.{PeerManager, _}
import encry.settings.EncryAppSettings
import encry.stats.{KafkaActor, LoggingActor, StatsSender, Zombie}
import encry.utils.{Logging, NetworkTimeProvider}
import encry.view.{EncryNodeViewHolder, ReadersHolder}
import org.encryfoundation.common.Algos
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

object EncryApp extends App with Logging {

  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  lazy val settings: EncryAppSettings = EncryAppSettings.read
  lazy val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)
  lazy val dbService: DBService = DBService()
  val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")
  val nodeId: Array[Byte] = Algos.hash(settings.network.nodeName
    .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)).take(5)
  lazy val basicSpecs = {
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    Seq(
      GetPeersSpec,
      PeersSpec,
      invSpec,
      requestModifierSpec,
      ModifiersSpec
    )
  }

  lazy val nodeViewHolder: ActorRef = system.actorOf(EncryNodeViewHolder.props(settings)
    .withDispatcher("nvh-dispatcher").withMailbox("nvh-mailbox"), "nodeViewHolder")
  val readersHolder: ActorRef = system.actorOf(Props[ReadersHolder], "readersHolder")
  lazy val networkController: ActorRef = system.actorOf(Props[NetworkController]
    .withDispatcher("network-dispatcher"), "networkController")
  lazy val peerManager: ActorRef = system.actorOf(Props[PeerManager], "peerManager")
  lazy val nodeViewSynchronizer: ActorRef =
    system.actorOf(Props(classOf[NodeViewSynchronizer]), "nodeViewSynchronizer")
  lazy val miner: ActorRef = system.actorOf(Props[Miner], "miner")
  if (settings.influxDB.isDefined) system.actorOf(Props[StatsSender], "statsSender")
  if (settings.kafka.exists(_.sendToKafka))
    system.actorOf(Props[KafkaActor].withDispatcher("kafka-dispatcher"), "kafkaActor")
  if (settings.postgres.exists(_.enableSave) || settings.postgres.exists(_.enableRestore) ) {
    if (settings.postgres.exists(_.enableSave))
      system.actorOf(Props(classOf[BlockListener], dbService, readersHolder, nodeViewHolder), "blockListener")
    if (settings.postgres.exists(_.enableRestore)) {
      system.actorOf(Props(classOf[PostgresRestore], dbService, nodeViewHolder), "postgresRestore")
      if (!settings.levelDb.exists(_.enableRestore)) system.actorSelection("/user/postgresRestore") ! StartRecovery
    }
  }
  if (settings.levelDb.exists(_.enableSave) || settings.levelDb.exists(_.enableRestore))
    system.actorOf(Props[ModifiersHolder], "modifiersHolder")
  if (settings.node.mining) miner ! StartMining
  if (settings.node.useCli) {
    system.actorOf(Props[ConsoleListener], "cliListener")
    system.actorSelection("/user/cliListener") ! StartListening
  }
  if (settings.node.loggingMode != "off") {
    system.actorOf(Props[LoggingActor], "loggingActor")
    system.actorOf(Props[Zombie], "zombie")
  }

  if (settings.restApi.enabled.getOrElse(false)) {
    import akka.http.scaladsl.model.StatusCodes._
    import akka.http.scaladsl.server.Directives._
    implicit def apiExceptionHandler: ExceptionHandler =
      ExceptionHandler {
        case e: Exception =>
          extractUri { uri =>
            logError(s"Request to $uri could not be handled normally due to: $e")
            complete(HttpResponse(InternalServerError, entity = "Internal server error"))
          }
      }

    val apiRoutes: Seq[ApiRoute] = Seq(
      UtilsApiRoute(settings.restApi),
      PeersApiRoute(peerManager, networkController, settings.restApi),
      InfoApiRoute(readersHolder, miner, peerManager, settings, nodeId, timeProvider),
      HistoryApiRoute(readersHolder, miner, settings, nodeId, settings.node.stateMode),
      TransactionsApiRoute(readersHolder, nodeViewHolder, settings.restApi, settings.node.stateMode),
      StateInfoApiRoute(readersHolder, nodeViewHolder, settings.restApi, settings.node.stateMode),
      WalletInfoApiRoute(nodeViewHolder, settings.restApi)
    )
    Http().bindAndHandle(
      CompositeHttpService(system, apiRoutes, settings.restApi, swaggerConfig).compositeRoute,
      settings.restApi.bindAddress.getAddress.getHostAddress,
      settings.restApi.bindAddress.getPort)
  }

  def forceStopApplication(code: Int = 0): Nothing = {
    system.registerOnTermination {
      println("Actor system terminated")
    }
    Await.ready(system.terminate(), 2 minutes)
    sys.exit(code)
  }

  def commonSupervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = 5,
    withinTimeRange = 60 seconds) {
    case _ => Restart
  }

}