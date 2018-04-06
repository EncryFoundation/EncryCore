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
import encry.view.{EncryNodeViewHolder, EncryViewReadersHolder}
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings
import scorex.core.utils.ScorexLogging

import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.concurrent.duration._
import scala.io.Source


class EncryApp(args: Seq[String]) extends Application {

  override type P = EncryProposition
  override type TX = EncryBaseTransaction
  override type PMOD = EncryPersistentModifier
  override type NVHT = EncryNodeViewHolder[_]

  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

  lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(args.headOption)

  override implicit lazy val settings: ScorexSettings = encrySettings.scorexSettings

  val nodeId: Array[Byte] = Algos.hash(encrySettings.scorexSettings.network.nodeName).take(5)

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(EncrySyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = EncryNodeViewHolder.createActor(actorSystem, encrySettings, timeProvider)

  val readersHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[EncryViewReadersHolder], nodeViewHolderRef))

  val minerRef: ActorRef = EncryMinerRef(encrySettings, nodeViewHolderRef, nodeId, timeProvider)

  val scannerRef: ActorRef = EncryScannerRef(encrySettings, nodeViewHolderRef)

  val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManagerRef, networkControllerRef, settings.restApi),
    InfoApiRoute(readersHolderRef, minerRef, peerManagerRef, encrySettings, nodeId),
    HistoryApiRoute(readersHolderRef, minerRef, encrySettings, nodeId, encrySettings.nodeSettings.stateMode),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, settings.restApi, encrySettings.nodeSettings.stateMode),
    AccountInfoApiRoute(readersHolderRef, nodeViewHolderRef, scannerRef, settings.restApi, encrySettings.nodeSettings.stateMode)
  )

  override val localInterface: ActorRef =
    EncryLocalInterfaceRef(nodeViewHolderRef, peerManagerRef, encrySettings, timeProvider)

  override val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(new EncryNodeViewSynchronizer(
      networkControllerRef, nodeViewHolderRef, localInterface, EncrySyncInfoMessageSpec, settings.network, timeProvider)))

  if (encrySettings.nodeSettings.mining && encrySettings.nodeSettings.offlineGeneration) {
    minerRef ! StartMining
  }

  if (encrySettings.testingSettings.transactionGeneration) {
    val txGen =
      actorSystem.actorOf(TransactionGenerator.props(nodeViewHolderRef, encrySettings.testingSettings, timeProvider))
    txGen ! StartGeneration
  }

  val cliListenerRef: ActorRef =
    actorSystem.actorOf(Props(classOf[ConsolePromptListener], nodeViewHolderRef, encrySettings))
  cliListenerRef ! StartListening

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

  sys.addShutdownHook(EncryApp.shutdown(actorSystem, allActors))
}

object EncryApp extends ScorexLogging {

  def main(args: Array[String]): Unit = new EncryApp(args).run()

  def forceStopApplication(code: Int = 1): Nothing = sys.exit(code)

  def shutdown(system: ActorSystem, actors: Seq[ActorRef]): Unit = {
    log.warn("Terminating Actors")
    actors.foreach{ a => a ! PoisonPill }
    log.warn("Terminating ActorSystem")
    val termination = system.terminate()
    Await.result(termination, 60 seconds)
    log.warn("Application has been terminated.")
  }
}
