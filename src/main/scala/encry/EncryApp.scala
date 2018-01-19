package encry

import akka.actor.{ActorRef, Props}
import encry.api.routes.{DebugApiRoute, HistoryApiRoute, MiningApiRoute, StateApiRoute}
import encry.local.TransactionGenerator.StartGeneration
import encry.local.mining.EncryMiner
import encry.local.mining.EncryMiner.StartMining
import encry.local.{EncryLocalInterface, TransactionGenerator}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.EncryBaseTransaction
import encry.network.EncryNodeViewSynchronizer
import encry.settings.{Algos, EncryAppSettings}
import encry.view.EncryNodeViewHolder
import encry.view.history.EncrySyncInfoMessageSpec
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.Proposition

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source


class EncryApp(args: Seq[String]) extends Application {

  override type P = Proposition
  override type TX = EncryBaseTransaction
  override type PMOD = EncryPersistentModifier
  override type NVHT = EncryNodeViewHolder[_]

  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

  lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(args.headOption)

  override implicit lazy val settings: ScorexSettings = encrySettings.scorexSettings

  val nodeId: Array[Byte] = Algos.hash(encrySettings.scorexSettings.network.nodeName).take(5)

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(EncrySyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = EncryNodeViewHolder.createActor(actorSystem, encrySettings)

  val minerRef: ActorRef = actorSystem.actorOf(Props(classOf[EncryMiner], nodeViewHolderRef, encrySettings, nodeId))

  val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  // TODO: Implement.
  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManagerRef, networkController, settings.restApi),
    HistoryApiRoute(nodeViewHolderRef, settings.restApi, encrySettings.nodeSettings.ADState),
    StateApiRoute(nodeViewHolderRef, settings.restApi, encrySettings.nodeSettings.ADState),
    MiningApiRoute(minerRef, settings.restApi),
    DebugApiRoute(nodeViewHolderRef, settings.restApi, nodeId)
  )

  override val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[EncryLocalInterface], nodeViewHolderRef, minerRef, encrySettings)
  )

  override val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(new EncryNodeViewSynchronizer(
      networkController, nodeViewHolderRef, localInterface, EncrySyncInfoMessageSpec, settings.network)))

  if (encrySettings.nodeSettings.mining && encrySettings.nodeSettings.offlineGeneration) {
    minerRef ! StartMining
  }

  if (encrySettings.testingSettings.transactionGeneration) {
    val txGen = actorSystem.actorOf(Props(classOf[TransactionGenerator], nodeViewHolderRef, encrySettings.testingSettings))
    txGen ! StartGeneration
  }
}

object EncryApp extends App {

  new EncryApp(args).run()

  def forceStopApplication(code: Int = 1): Unit =
    new Thread(() => System.exit(code), "encry-shutdown-thread").start()
}
