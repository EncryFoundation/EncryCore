package encry

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.stream.ActorMaterializer
import encry.api.http.routes.{AccountInfoApiRoute, HistoryApiRoute, InfoApiRoute, TransactionsApiRoute}
import encry.cli.ConsolePromptListener
import encry.cli.ConsolePromptListener.StartListening
import encry.local.TransactionGenerator.StartGeneration
import encry.local.miner.EncryMiner.StartMining
import encry.local.miner.EncryMinerRef
import encry.local.scanner.EncryScannerRef
import encry.local.{EncryLocalInterfaceRef, TransactionGenerator}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proposition.EncryProposition
import encry.network.EncryNodeViewSynchronizer
import encry.settings.{Algos, EncryAppSettings}
import encry.view.history.EncrySyncInfoMessageSpec
import encry.view.{EncryNodeViewHolder, EncryNodeViewHolderRef, EncryReadersHolderRef}
import scorex.core.api.http._
import scorex.core.network.{NetworkControllerRef, UPnP}
import scorex.core.network.message._
import scorex.core.settings.ScorexSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.core.network.NetworkController.ReceivableMessages.ShutdownNetwork
import scorex.core.network.peer.PeerManagerRef

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source


class EncryApp(args: Seq[String]) extends ScorexLogging {

  type P = EncryProposition
  type TX = EncryBaseTransaction
  type PMOD = EncryPersistentModifier
  type NVHT = EncryNodeViewHolder[_]

  lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(args.headOption)

  implicit lazy val settings: ScorexSettings = encrySettings.scorexSettings

  implicit def exceptionHandler: ExceptionHandler = ApiErrorHandler.exceptionHandler
  implicit val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

  val timeProvider = new NetworkTimeProvider(settings.ntp)
  val peerManagerRef = PeerManagerRef(settings, timeProvider)
  lazy val combinedRoute: Route = CompositeHttpService(actorSystem, apiRoutes, settings.restApi, swaggerConfig).compositeRoute

  def run(): Unit = {
    require(settings.network.agentName.length <= 50)

    implicit val materializer: ActorMaterializer = ActorMaterializer()
    val bindAddress = settings.restApi.bindAddress

    Http().bindAndHandle(combinedRoute, bindAddress.getAddress.getHostAddress, bindAddress.getPort)

  }

  val nodeId: Array[Byte] = Algos.hash(encrySettings.scorexSettings.network.nodeName).take(5)

  private lazy val basicSpecs = {
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

  protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(EncrySyncInfoMessageSpec)

  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)

  val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManagerRef, networkController, settings.restApi),
    InfoApiRoute(readersHolder, minerRef, peerManagerRef, encrySettings, nodeId, timeProvider),
    HistoryApiRoute(readersHolder, minerRef, encrySettings, nodeId, encrySettings.nodeSettings.stateMode),
    TransactionsApiRoute(readersHolder, nodeViewHolder, settings.restApi, encrySettings.nodeSettings.stateMode),
    AccountInfoApiRoute(readersHolder, nodeViewHolder, scanner, settings.restApi, encrySettings.nodeSettings.stateMode)
  )

  //Акторы

  val localInterface: ActorRef =
    EncryLocalInterfaceRef(nodeViewHolder, peerManagerRef, encrySettings, timeProvider)

  val nodeViewSynchronizer: ActorRef =
    EncryNodeViewSynchronizer(networkController, nodeViewHolder, EncrySyncInfoMessageSpec, settings.network, timeProvider)

  val cliListener: ActorRef =
    actorSystem.actorOf(Props(classOf[ConsolePromptListener], nodeViewHolder, encrySettings, minerRef))

  val nodeViewHolder: ActorRef = EncryNodeViewHolderRef(encrySettings, timeProvider)

  val readersHolder: ActorRef = EncryReadersHolderRef(nodeViewHolder)

  val minerRef: ActorRef = EncryMinerRef(encrySettings, nodeViewHolder, readersHolder, nodeId, timeProvider)

  val scanner: ActorRef = EncryScannerRef(encrySettings, nodeViewHolder)

  val networkController: ActorRef = NetworkControllerRef("networkController",settings.network,
    messagesHandler, upnp, peerManagerRef, timeProvider)

  //--------

  if (encrySettings.nodeSettings.mining && encrySettings.nodeSettings.offlineGeneration) minerRef ! StartMining

  if (encrySettings.testingSettings.transactionGeneration) {
    val txGen =
      actorSystem.actorOf(TransactionGenerator.props(nodeViewHolder, encrySettings.testingSettings, timeProvider))
    txGen ! StartGeneration
  }

  if (encrySettings.nodeSettings.enableCLI) cliListener ! StartListening

  lazy val upnp = new UPnP(settings.network)

  def stopAll(): Unit = synchronized {
    log.info("Stopping network services")
    if (settings.network.upnpEnabled) upnp.deletePort(settings.network.bindAddress.getPort)
    networkController ! ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>

      log.info("Exiting from the app...")
      System.exit(0)
    }
  }
}

object EncryApp extends ScorexLogging {

  def main(args: Array[String]): Unit = new EncryApp(args).run()

  def forceStopApplication(code: Int = 0): Nothing = sys.exit(code)

  def shutdown(system: ActorSystem, actors: Seq[ActorRef]): Unit = {
    log.warn("Terminating Actors")
    actors.foreach(_ ! PoisonPill)
    log.warn("Terminating ActorSystem")
    system.terminate()
  }
}