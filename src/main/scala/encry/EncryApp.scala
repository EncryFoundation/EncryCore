package encry

import java.net.InetSocketAddress

import akka.actor.SupervisorStrategy.Escalate
import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import encry.api.http.routes.{AccountInfoApiRoute, HistoryApiRoute, InfoApiRoute, TransactionsApiRoute}
import encry.cli.ConsolePromptListener
import encry.cli.ConsolePromptListener.StartListening
import encry.local.TransactionGenerator.StartGeneration
import encry.local.miner.EncryMiner
import encry.local.miner.EncryMiner.StartMining
import encry.local.scanner.EncryScanner
import encry.local.TransactionGenerator
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proposition.EncryProposition
import encry.network.{EncryNodeViewSynchronizer, NetworkController, PeerSynchronizer}
import encry.settings.{Algos, EncryAppSettings}
import encry.view.history.EncrySyncInfoMessageSpec
import encry.view.{EncryNodeViewHolder, EncryViewReadersHolder}
import scorex.core.api.http._
import scorex.core.network.UPnP
import scorex.core.network.message._
import scorex.core.settings.ScorexSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import encry.network.peer.PeerManager

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source
import scala.concurrent.duration._

object EncryApp extends App with ScorexLogging {

  type P = EncryProposition
  type TX = EncryBaseTransaction
  type PMOD = EncryPersistentModifier
  type NVHT = EncryNodeViewHolder[_]

  lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(args.headOption)

  implicit lazy val settings: ScorexSettings = encrySettings.scorexSettings

  implicit val system: ActorSystem = ActorSystem(settings.network.agentName)
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  require(settings.network.agentName.length <= 50)
  lazy val bindAddress: InetSocketAddress = settings.restApi.bindAddress

  lazy val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)
  val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  val nodeId: Array[Byte] = Algos.hash(encrySettings.scorexSettings.network.nodeName).take(5)

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

  lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(EncrySyncInfoMessageSpec)

  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)

  lazy val peerManager: ActorRef = system.actorOf(Props[PeerManager], "peerManager")

  lazy val nodeViewHolder: ActorRef = system.actorOf(EncryNodeViewHolder.props(), "nodeViewHolder")

  val readersHolder: ActorRef = system.actorOf(Props[EncryViewReadersHolder], "readersHolder")

  lazy val networkController: ActorRef = system.actorOf(Props[NetworkController], "networkController")

  val peerSynchronizer: ActorRef = system.actorOf(Props[PeerSynchronizer], "peerSynchronizer")

  val nodeViewSynchronizer: ActorRef =
    system.actorOf(Props(classOf[EncryNodeViewSynchronizer], EncrySyncInfoMessageSpec), "nodeViewSynchronizer")

  lazy val miner: ActorRef = system.actorOf(Props[EncryMiner], "miner")

  val cliListener: ActorRef = system.actorOf(Props[ConsolePromptListener], "cliListener")

  val scanner: ActorRef = system.actorOf(EncryScanner.props(), "scanner")

  val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManager, networkController, settings.restApi),
    InfoApiRoute(readersHolder, miner, peerManager, encrySettings, nodeId, timeProvider),
    HistoryApiRoute(readersHolder, miner, encrySettings, nodeId, encrySettings.nodeSettings.stateMode),
    TransactionsApiRoute(readersHolder, nodeViewHolder, settings.restApi, encrySettings.nodeSettings.stateMode),
    AccountInfoApiRoute(readersHolder, nodeViewHolder, scanner, settings.restApi, encrySettings.nodeSettings.stateMode)
  )

  val combinedRoute: Route = CompositeHttpService(system, apiRoutes, settings.restApi, swaggerConfig).compositeRoute
  Http().bindAndHandle(combinedRoute, bindAddress.getAddress.getHostAddress, bindAddress.getPort)

  lazy val upnp: UPnP = new UPnP(settings.network)

  def commonSupervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = 5,
    withinTimeRange = 60 seconds) {
    case _ => Escalate
  }

  if (encrySettings.nodeSettings.mining && encrySettings.nodeSettings.offlineGeneration) miner ! StartMining

  if (encrySettings.testingSettings.transactionGeneration) {
    val transactionGenerator: ActorRef = system.actorOf(Props[TransactionGenerator], "tx-generator")
    transactionGenerator ! StartGeneration
  }

  if (encrySettings.nodeSettings.enableCLI) cliListener ! StartListening

  def forceStopApplication(code: Int = 0): Nothing = sys.exit(code)
}