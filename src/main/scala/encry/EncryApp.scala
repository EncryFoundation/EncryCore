package encry

import java.net.InetSocketAddress

import akka.actor.SupervisorStrategy.Escalate
import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import encry.api.http.routes.{HistoryApiRoute, InfoApiRoute, StateInfoApiRoute, TransactionsApiRoute}
import encry.api.http.{ApiRoute, CompositeHttpService, PeersApiRoute, UtilsApiRoute}
import encry.cli.ConsolePromptListener
import encry.cli.ConsolePromptListener.StartListening
import encry.local.TransactionGenerator
import encry.local.TransactionGenerator.StartGeneration
import encry.local.explorer.BlockListener
import encry.local.miner.EncryMiner
import encry.local.miner.EncryMiner.StartMining
import encry.local.scanner.EncryScanner
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.EncryProposition
import encry.network.message._
import encry.network.peer.PeerManager
import encry.network.{EncryNodeViewSynchronizer, NetworkController, UPnP}
import encry.settings.{Algos, EncryAppSettings}
import encry.stats.StatsSender
import encry.utils.{NetworkTimeProvider, ScorexLogging}
import encry.view.history.EncrySyncInfoMessageSpec
import encry.view.{EncryNodeViewHolder, EncryViewReadersHolder}

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.io.Source

object EncryApp extends App with ScorexLogging {

  type P = EncryProposition
  type TX = EncryBaseTransaction
  type PMOD = EncryPersistentModifier
  type NVHT = EncryNodeViewHolder[_]

  lazy val settings: EncryAppSettings = EncryAppSettings.read

  implicit val system: ActorSystem = ActorSystem(settings.network.agentName)
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  require(settings.network.agentName.length <= 50)
  lazy val bindAddress: InetSocketAddress = settings.restApi.bindAddress

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

  lazy val networkController: ActorRef = system.actorOf(Props[NetworkController], "networkController")

  lazy val peerManager: ActorRef = system.actorOf(Props[PeerManager], "peerManager")

  lazy val nodeViewSynchronizer: ActorRef =
    system.actorOf(Props(classOf[EncryNodeViewSynchronizer], EncrySyncInfoMessageSpec), "nodeViewSynchronizer")

  lazy val miner: ActorRef = system.actorOf(Props[EncryMiner], "miner")

  val blockListener: ActorRef = system.actorOf(Props[BlockListener], "blockListener")

  val cliListener: ActorRef = system.actorOf(Props[ConsolePromptListener], "cliListener")

  val scanner: ActorRef = system.actorOf(EncryScanner.props(), "scanner")

  val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManager, networkController, settings.restApi),
    InfoApiRoute(readersHolder, miner, peerManager, settings, nodeId, timeProvider),
    HistoryApiRoute(readersHolder, miner, settings, nodeId, settings.node.stateMode),
    TransactionsApiRoute(readersHolder, nodeViewHolder, settings.restApi, settings.node.stateMode),
    StateInfoApiRoute(readersHolder, nodeViewHolder, scanner, settings.restApi, settings.node.stateMode)
  )

  val combinedRoute: Route = CompositeHttpService(system, apiRoutes, settings.restApi, swaggerConfig).compositeRoute
  Http().bindAndHandle(combinedRoute, bindAddress.getAddress.getHostAddress, bindAddress.getPort)

  lazy val upnp: UPnP = new UPnP(settings.network)

  def commonSupervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = 5,
    withinTimeRange = 60 seconds) {
    case _ => Escalate
  }

  if (settings.node.sendStat) system.actorOf(Props[StatsSender], "statsSender")

  if (settings.node.mining && settings.node.offlineGeneration) miner ! StartMining

  if (settings.testing.transactionGeneration) system.actorOf(Props[TransactionGenerator], "tx-generator") ! StartGeneration

  if (settings.node.enableCLI) cliListener ! StartListening

  def forceStopApplication(code: Int = 0): Nothing = sys.exit(code)
}