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
import encry.local.TransactionGenerator.StartGeneration
import encry.local.miner.EncryMiner
import encry.local.miner.EncryMiner.StartMining
import encry.network.message._
import encry.network.peer.PeerManager
import encry.network.{EncryNodeViewSynchronizer, ModifiersHolder, NetworkController, UPnP}
import encry.settings.{Algos, EncryAppSettings}
import encry.stats.StatsSender
import encry.utils.{Logging, NetworkTimeProvider}
import encry.view.history.EncrySyncInfoMessageSpec
import encry.view.{EncryNodeViewHolder, EncryViewReadersHolder}
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

object EncryApp extends App with Logging {

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
  lazy val nodeViewHolder: ActorRef = system.actorOf(EncryNodeViewHolder.props(), "nodeViewHolder")
  val readersHolder: ActorRef = system.actorOf(Props[EncryViewReadersHolder], "readersHolder")
  lazy val networkController: ActorRef = system.actorOf(Props[NetworkController]
    .withDispatcher("network-dispatcher"), "networkController")
  lazy val peerManager: ActorRef = system.actorOf(Props[PeerManager], "peerManager")
  lazy val nodeViewSynchronizer: ActorRef =
    system.actorOf(Props(classOf[EncryNodeViewSynchronizer], EncrySyncInfoMessageSpec), "nodeViewSynchronizer")
  lazy val miner: ActorRef = system.actorOf(Props[EncryMiner].withDispatcher("mining-dispatcher"), "miner")
  val cliListener: ActorRef = system.actorOf(Props[ConsolePromptListener], "cliListener")

  lazy val upnp: UPnP = new UPnP(settings.network)

  if (settings.restApi.enabled) {

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
      StateInfoApiRoute(readersHolder, nodeViewHolder, settings.restApi, settings.node.stateMode)
    )
    val combinedRoute: Route = CompositeHttpService(system, apiRoutes, settings.restApi, swaggerConfig).compositeRoute
    Http().bindAndHandle(combinedRoute, settings.restApi.bindAddress.getAddress.getHostAddress, settings.restApi.bindAddress.getPort)
  }

  if (settings.node.sendStat) system.actorOf(Props[StatsSender], "statsSender")
  if (settings.node.mining) miner ! StartMining
  if (settings.levelDb.enable) system.actorOf(Props[ModifiersHolder], "modifiersHolder")
  if (settings.testing.transactionGeneration) system.actorOf(Props[TransactionGenerator], "tx-generator") ! StartGeneration
  if (settings.node.enableCLI) cliListener ! StartListening

  def forceStopApplication(code: Int = 0): Nothing = sys.exit(code)

  def commonSupervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = 5,
    withinTimeRange = 60 seconds) {
    case _ => Restart
  }
}