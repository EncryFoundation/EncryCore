package encry

import akka.actor.ActorRef
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.EncryBaseTransaction
import encry.settings.EncryAppSettings
import encry.view.EncryNodeViewHolder
import encry.view.history.EncrySyncInfoMessageSpec
import scorex.core.api.http.ApiRoute
import scorex.core.app.Application
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.Proposition


class EncryApp(args: Seq[String]) extends Application {

  override type P = Proposition
  override type TX = EncryBaseTransaction
  override type PMOD = EncryPersistentModifier
  override type NVHT = EncryNodeViewHolder[_]

  // TODO: Add `ScorexSettings` to the `EncryAppSettings`.
  override implicit val settings: ScorexSettings = _

  lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(args.headOption)

  override protected val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(EncrySyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = EncryNodeViewHolder.createActor(actorSystem, encrySettings)

  override val nodeViewSynchronizer: ActorRef = _

  override val swaggerConfig: String = _

  override val apiRoutes: Seq[ApiRoute] = _

  override val localInterface: ActorRef = _
}

object EncryApp extends App {

  new EncryApp(args).run()

  def forceStopApplication(code: Int = 1): Unit =
    new Thread(() => System.exit(code), "encry-shutdown-thread").start()
}