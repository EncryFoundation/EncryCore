package encry

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.stream.ActorMaterializer
import scorex.core.api.http.{ApiErrorHandler, ApiRoute, CompositeHttpService}
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.network.peer.PeerManagerRef
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.core.{NodeViewHolder, PersistentNodeViewModifier}

import scala.concurrent.ExecutionContext.Implicits.global

trait Application extends ScorexLogging {

  //settings
  implicit val settings: ScorexSettings
  //api
  val apiRoutes: Seq[ApiRoute]
  implicit def exceptionHandler: ExceptionHandler = ApiErrorHandler.exceptionHandler
  implicit val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  //p2p

  val swaggerConfig: String
  val timeProvider = new NetworkTimeProvider(settings.ntp)
  val peerManagerRef = PeerManagerRef(settings, timeProvider)
  lazy val combinedRoute: Route = CompositeHttpService(actorSystem, apiRoutes, settings.restApi, swaggerConfig).compositeRoute

  def run(): Unit = {
    require(settings.network.agentName.length <= 50)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at ${settings.restApi.bindAddress.toString}")

    implicit val materializer: ActorMaterializer = ActorMaterializer()
    val bindAddress = settings.restApi.bindAddress

    Http().bindAndHandle(combinedRoute, bindAddress.getAddress.getHostAddress, bindAddress.getPort)

  }


}
