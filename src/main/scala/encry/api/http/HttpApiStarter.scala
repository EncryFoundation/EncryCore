package encry.api.http

import java.net.InetAddress
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.server.ExceptionHandler
import akka.stream.ActorMaterializer
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.routes._
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.Algos
import scala.io.Source

object HttpApiStarter extends StrictLogging {

  def startHttpServer(settings: EncryAppSettings,
                      peerManager: ActorRef,
                      networkController: ActorRef,
                      readersHolder: ActorRef,
                      miner: ActorRef,
                      memoryPool: ActorRef,
                      timeProvider: NetworkTimeProvider,
                      nodeViewHolder: ActorRef)(implicit system: ActorSystem): Unit = {

    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")
    val nodeId: Array[Byte] = Algos.hash(settings.network.nodeName
      .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)).take(5)

    if (settings.restApi.enabled.getOrElse(false)) {
      import akka.http.scaladsl.model.StatusCodes._
      import akka.http.scaladsl.server.Directives._
      implicit def apiExceptionHandler: ExceptionHandler =
        ExceptionHandler {
          case e: Exception =>
            extractUri { uri =>
              logger.info(s"Request to $uri could not be handled normally due to: $e")
              complete(HttpResponse(InternalServerError, entity = "Internal server error"))
            }
        }

      val apiRoutes: Seq[ApiRoute] = Seq(
        UtilsApiRoute(settings.restApi),
        PeersApiRoute(peerManager, networkController, settings.restApi),
        InfoApiRoute(readersHolder, miner, peerManager, settings, nodeId, memoryPool, timeProvider),
        HistoryApiRoute(readersHolder, miner, settings, nodeId, settings.node.stateMode),
        TransactionsApiRoute(readersHolder, memoryPool, settings.restApi, settings.node.stateMode),
        StateInfoApiRoute(readersHolder, nodeViewHolder, settings.restApi, settings.node.stateMode),
        WalletInfoApiRoute(nodeViewHolder, settings.restApi)
      )
      logger.info(s"Starting http server on ${settings.restApi.bindAddress.getAddress.getHostAddress}" +
        s":${ settings.restApi.bindAddress.getPort}")
      Http().bindAndHandle(
        CompositeHttpService(system, apiRoutes, settings.restApi, swaggerConfig).compositeRoute,
        settings.restApi.bindAddress.getAddress.getHostAddress,
        settings.restApi.bindAddress.getPort)
    }
  }
}