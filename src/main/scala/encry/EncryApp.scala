package encry

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.stream.ActorMaterializer
import encry.api.http.routes.{HistoryApiRoute, InfoApiRoute, StateInfoApiRoute, TransactionsApiRoute}
import encry.api.http.{ApiRoute, CompositeHttpService, PeersApiRoute, UtilsApiRoute}
import encry.cli.ConsolePromptListener
import encry.cli.ConsolePromptListener.StartListening
import encry.local.TransactionGenerator
import encry.local.explorer.BlockListener
import encry.local.explorer.database.DBService
import encry.local.miner.Miner
import encry.local.miner.Miner.StartMining
import encry.network.message._
import encry.network.peer.PeerManager
import encry.network.{EncryNodeViewSynchronizer, ModifiersHolder, NetworkController}
import encry.settings.EncryAppSettings
import encry.stats.{KafkaActor, LoggingActor, StatsSender}
import encry.stats.LoggingActor.LogMessage
import encry.utils.{NetworkTimeProvider, Zombie}
import encry.view.history.EncrySyncInfoMessageSpec
import encry.view.{EncryNodeViewHolder, EncryViewReadersHolder}
import org.encryfoundation.common.Algos
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

object EncryApp extends App {

  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  lazy val settings: EncryAppSettings = EncryAppSettings.read
  lazy val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)
  val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")
  val nodeId: Array[Byte] = Algos.hash(settings.network.nodeName).take(5)
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

  lazy val nodeViewHolder: ActorRef = system.actorOf(EncryNodeViewHolder.props()
    .withDispatcher("nvh-dispatcher").withMailbox("nvh-mailbox"), "nodeViewHolder")
  val readersHolder: ActorRef = system.actorOf(Props[EncryViewReadersHolder], "readersHolder")
  lazy val networkController: ActorRef = system.actorOf(Props[NetworkController]
    .withDispatcher("network-dispatcher"), "networkController")
  lazy val peerManager: ActorRef = system.actorOf(Props[PeerManager], "peerManager")
  lazy val nodeViewSynchronizer: ActorRef =
    system.actorOf(Props(classOf[EncryNodeViewSynchronizer], EncrySyncInfoMessageSpec), "nodeViewSynchronizer")
  lazy val miner: ActorRef = system.actorOf(Props[Miner], "miner")
  val cliListener: ActorRef = system.actorOf(Props[ConsolePromptListener], "cliListener")
  if (settings.node.sendStat) system.actorOf(Props[StatsSender], "statsSender")
  if (settings.kafka.sendToKafka) system.actorOf(Props[KafkaActor].withDispatcher("kafka-dispatcher"), "kafkaActor")
  if (settings.node.mining && settings.node.offlineGeneration) miner ! StartMining
  if (settings.postgres.enabled) system.actorOf(Props(classOf[BlockListener], DBService()), "blockListener")
  if (settings.node.mining) miner ! StartMining
  if (settings.levelDb.enable) system.actorOf(Props[ModifiersHolder], "modifiersHolder")
  if (settings.testing.transactionGeneration)
    system.actorOf(Props[TransactionGenerator].withDispatcher("transaction-generator-dispatcher"), "tx-generator")
  if (settings.node.enableCLI) cliListener ! StartListening
  system.actorOf(Props[Zombie], "zombie")
  if (settings.logging.enableLogging) system.actorOf(Props[LoggingActor], "loggingActor")

  if (settings.restApi.enabled) {
    import akka.http.scaladsl.model.StatusCodes._
    import akka.http.scaladsl.server.Directives._
    implicit def apiExceptionHandler: ExceptionHandler =
      ExceptionHandler {
        case e: Exception =>
          extractUri { uri =>
            if (settings.logging.enableLogging) system.actorSelection("user/loggingActor") !
              LogMessage("Error", s"Request to $uri could not be handled normally due to: $e", System.currentTimeMillis())
            complete(HttpResponse(InternalServerError, entity = "Internal server error"))
          }
      }

    val apiRoutes: Seq[ApiRoute] = Seq(
      UtilsApiRoute(settings.restApi),
      PeersApiRoute(peerManager, networkController, settings.restApi),
      InfoApiRoute(readersHolder, miner, peerManager, settings, nodeId, timeProvider),
      HistoryApiRoute(readersHolder, miner, settings, nodeId, settings.node.stateMode),
      TransactionsApiRoute(readersHolder, nodeViewHolder, settings.restApi, settings.node.stateMode),
      StateInfoApiRoute(readersHolder, nodeViewHolder, settings.restApi, settings.node.stateMode)
    )
    val combinedRoute: Route = CompositeHttpService(system, apiRoutes, settings.restApi, swaggerConfig).compositeRoute
    Http().bindAndHandle(combinedRoute, settings.restApi.bindAddress.getAddress.getHostAddress, settings.restApi.bindAddress.getPort)
  }

  def forceStopApplication(code: Int = 0): Nothing = sys.exit(code)

  def commonSupervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = 5,
    withinTimeRange = 60 seconds) {
    case _ => Restart
  }
}