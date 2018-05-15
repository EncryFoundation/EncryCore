package encry

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
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
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.network.{NetworkControllerRef, UPnP}
import scorex.core.network.message._
import scorex.core.settings.ScorexSettings
import scorex.core.utils.ScorexLogging
import scorex.core.network.NetworkController.ReceivableMessages.ShutdownNetwork

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source


class EncryApp(args: Seq[String]) extends Application {

  type P = EncryProposition
  type TX = EncryBaseTransaction
  type PMOD = EncryPersistentModifier
  type NVHT = EncryNodeViewHolder[_]

  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

  lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(args.headOption)

  override implicit lazy val settings: ScorexSettings = encrySettings.scorexSettings

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

  val nodeViewHolderRef: ActorRef = EncryNodeViewHolderRef(encrySettings, timeProvider)

  val readersHolderRef: ActorRef = EncryReadersHolderRef(nodeViewHolderRef)

  val minerRef: ActorRef = EncryMinerRef(encrySettings, nodeViewHolderRef, readersHolderRef, nodeId, timeProvider)

  val scannerRef: ActorRef = EncryScannerRef(encrySettings, nodeViewHolderRef)

  val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  val networkControllerRef: ActorRef = NetworkControllerRef("networkController",settings.network,
    messagesHandler, upnp, peerManagerRef, timeProvider)

  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManagerRef, networkControllerRef, settings.restApi),
    InfoApiRoute(readersHolderRef, minerRef, peerManagerRef, encrySettings, nodeId, timeProvider),
    HistoryApiRoute(readersHolderRef, minerRef, encrySettings, nodeId, encrySettings.nodeSettings.stateMode),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, settings.restApi, encrySettings.nodeSettings.stateMode),
    AccountInfoApiRoute(readersHolderRef, nodeViewHolderRef, scannerRef, settings.restApi, encrySettings.nodeSettings.stateMode)
  )

  val localInterface: ActorRef =
    EncryLocalInterfaceRef(nodeViewHolderRef, peerManagerRef, encrySettings, timeProvider)

  val nodeViewSynchronizer: ActorRef =
    EncryNodeViewSynchronizer(networkControllerRef, nodeViewHolderRef, EncrySyncInfoMessageSpec, settings.network, timeProvider)

  val cliListenerRef: ActorRef =
    actorSystem.actorOf(Props(classOf[ConsolePromptListener], nodeViewHolderRef, encrySettings, minerRef))

  if (encrySettings.nodeSettings.mining && encrySettings.nodeSettings.offlineGeneration) minerRef ! StartMining

  if (encrySettings.testingSettings.transactionGeneration) {
    val txGen =
      actorSystem.actorOf(TransactionGenerator.props(nodeViewHolderRef, encrySettings.testingSettings, timeProvider))
    txGen ! StartGeneration
  }

  if (encrySettings.nodeSettings.enableCLI) cliListenerRef ! StartListening

  val allActors = Seq(
    nodeViewHolderRef,
    nodeViewSynchronizer,
    readersHolderRef,
    networkControllerRef,
    localInterface,
    cliListenerRef,
    scannerRef,
    minerRef
  )

  lazy val upnp = new UPnP(settings.network)

  def stopAll(): Unit = synchronized {
    log.info("Stopping network services")
    if (settings.network.upnpEnabled) upnp.deletePort(settings.network.bindAddress.getPort)
    networkControllerRef ! ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>

      log.info("Exiting from the app...")
      System.exit(0)
    }
  }

  //sys.addShutdownHook(EncryApp.shutdown(actorSystem, allActors))
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