package encry

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.stream.ActorMaterializer
import encry.api.http.routes.{AccountInfoApiRoute, HistoryApiRoute, InfoApiRoute, TransactionsApiRoute}
import encry.cli.ConsolePromptListener
import encry.cli.ConsolePromptListener.StartListening
import encry.local.TransactionGenerator.StartGeneration
import encry.local.miner.EncryMiner
import encry.local.miner.EncryMiner.StartMining
import encry.local.scanner.EncryScannerRef
import encry.local.{EncryLocalInterfaceRef, TransactionGenerator}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proposition.EncryProposition
import encry.network.EncryNodeViewSynchronizer
import encry.settings.{Algos, EncryAppSettings}
import encry.view.history.EncrySyncInfoMessageSpec
import encry.view.{EncryNodeViewHolder, EncryReadersHolderRef}
import scorex.core.api.http._
import scorex.core.network.{NetworkControllerRef, UPnP}
import scorex.core.network.message._
import scorex.core.settings.ScorexSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.core.network.peer.PeerManagerRef

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source

object EncryApp extends App with ScorexLogging {

  type P = EncryProposition
  type TX = EncryBaseTransaction
  type PMOD = EncryPersistentModifier
  type NVHT = EncryNodeViewHolder[_]

  lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(args.headOption)

  implicit val settings: ScorexSettings = encrySettings.scorexSettings

  implicit def exceptionHandler: ExceptionHandler = ApiErrorHandler.exceptionHandler

  implicit val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

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

  val peerManager: ActorRef = PeerManagerRef(settings, timeProvider)

  lazy val nodeViewHolder: ActorRef = actorSystem.actorOf(EncryNodeViewHolder.props(), "nodeViewHolder")

  val readersHolder: ActorRef = EncryReadersHolderRef(nodeViewHolder)

  val networkController: ActorRef = NetworkControllerRef("networkController", settings.network,
    messagesHandler, upnp, peerManager, timeProvider)

  val localInterface: ActorRef = EncryLocalInterfaceRef(nodeViewHolder, peerManager, encrySettings, timeProvider)

  val nodeViewSynchronizer: ActorRef =
    EncryNodeViewSynchronizer(networkController, nodeViewHolder, EncrySyncInfoMessageSpec, settings.network, timeProvider)

  lazy val miner: ActorRef = actorSystem.actorOf(EncryMiner.props(nodeId, timeProvider))

  val cliListener: ActorRef = actorSystem.actorOf(Props[ConsolePromptListener], "cliListener")

  val scanner: ActorRef = EncryScannerRef(encrySettings, nodeViewHolder)

  val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManager, networkController, settings.restApi),
    InfoApiRoute(readersHolder, miner, peerManager, encrySettings, nodeId, timeProvider),
    HistoryApiRoute(readersHolder, miner, encrySettings, nodeId, encrySettings.nodeSettings.stateMode),
    TransactionsApiRoute(readersHolder, nodeViewHolder, settings.restApi, encrySettings.nodeSettings.stateMode),
    AccountInfoApiRoute(readersHolder, nodeViewHolder, scanner, settings.restApi, encrySettings.nodeSettings.stateMode)
  )

  val combinedRoute: Route = CompositeHttpService(actorSystem, apiRoutes, settings.restApi, swaggerConfig).compositeRoute
  Http().bindAndHandle(combinedRoute, bindAddress.getAddress.getHostAddress, bindAddress.getPort)

  if (encrySettings.nodeSettings.mining && encrySettings.nodeSettings.offlineGeneration) miner ! StartMining

  if (encrySettings.testingSettings.transactionGeneration) {
    val txGen =
      actorSystem.actorOf(TransactionGenerator.props(nodeViewHolder, encrySettings.testingSettings, timeProvider))
    txGen ! StartGeneration
  }

  if (encrySettings.nodeSettings.enableCLI) cliListener ! StartListening

  lazy val upnp = new UPnP(settings.network)

  def forceStopApplication(code: Int = 0): Nothing = sys.exit(code)
}