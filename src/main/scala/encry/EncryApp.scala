package encry

import java.io.File
import java.net.InetAddress
import java.nio.file.Files
import akka.actor.SupervisorStrategy.Restart
import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.server.ExceptionHandler
import akka.stream.ActorMaterializer
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.routes._
import encry.api.http.{ApiRoute, CompositeHttpService}
import encry.settings.EncryAppSettings
import encry.stats.Zombie
import encry.utils.NetworkTimeProvider
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import kamon.system.SystemMetrics
import org.encryfoundation.common.utils.Algos
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.io.Source
import scala.language.postfixOps

object EncryApp extends App with StrictLogging {

  implicit val system: ActorSystem             = ActorSystem()
  implicit val ec: ExecutionContextExecutor    = system.dispatcher
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val isStateExists: Boolean = Files.exists(new File(s"${settings.directory}/state").toPath)

  lazy val settings: EncryAppSettings   = EncryAppSettings.read(args.headOption, isStateExists)
  val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

  val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")
  val nodeId: Array[Byte] = Algos
    .hash(
      settings.network.nodeName
        .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)
    )
    .take(5)

  val starter = system.actorOf(Props(new Starter(settings, timeProvider, nodeId, isStateExists, args.headOption)))
  if (settings.monitoringSettings.exists(_.kamonEnabled)) {
    Kamon.reconfigure(EncryAppSettings.allConfig)
    Kamon.addReporter(new InfluxDBReporter())
    SystemMetrics.startCollecting()
  }

  def configServer(starter: ActorRef): Future[Http.ServerBinding] = {
    import akka.http.scaladsl.model.StatusCodes._
    import akka.http.scaladsl.server.Directives._

    implicit def apiExceptionHandler: ExceptionHandler = ExceptionHandler {
      case e: Exception =>
        extractUri { uri =>
          logger.info(s"Request to $uri could not be handled normally due to: $e")
          complete(HttpResponse(InternalServerError, entity = "Internal server error"))
        }
    }

    val apiRoutes: Seq[ApiRoute] = Seq(
      ConfigRoute(settings.restApi, starter),
      ArgonRoute(settings.restApi)
    )
    Http().bindAndHandle(
      CompositeHttpService(system, apiRoutes, settings.restApi, swaggerConfig).compositeRoute,
      settings.restApi.bindAddress.getAddress.getHostAddress,
      settings.restApi.bindAddress.getPort
    )
  }

  def startHttp(dataHolderForApi: ActorRef, memoryPool: ActorRef) =
    if (settings.restApi.enabled.getOrElse(false)) {
      import akka.http.scaladsl.model.StatusCodes._
      import akka.http.scaladsl.server.Directives._

      implicit def apiExceptionHandler: ExceptionHandler = ExceptionHandler {
        case e: Exception =>
          extractUri { uri =>
            logger.info(s"Request to $uri could not be handled normally due to: $e")
            complete(HttpResponse(InternalServerError, entity = "Internal server error"))
          }
      }

      val apiRoutes: Seq[ApiRoute] = Seq(
        WebRoute(settings.restApi, settings.node, dataHolderForApi),
        WalletRoute(settings.restApi, dataHolderForApi, settings),
        PeersRoute(settings.restApi, settings.node, dataHolderForApi),
        PeersConnectedRoute(settings.restApi, dataHolderForApi),
        BanPeersRoute(settings.restApi, dataHolderForApi),
        ArgonRoute(settings.restApi),
        PeersApiRoute(settings.restApi, dataHolderForApi),
        InfoApiRoute(dataHolderForApi, settings.restApi, nodeId, timeProvider),
        HistoryApiRoute(dataHolderForApi, settings.restApi, nodeId),
        TransactionsApiRoute(dataHolderForApi, memoryPool, settings.restApi),
        WalletInfoApiRoute(dataHolderForApi, settings.restApi, Algos.encode(settings.constants.IntrinsicTokenId)),
        NodeRoute(dataHolderForApi, settings.restApi)
      )
      Http().bindAndHandle(
        CompositeHttpService(system, apiRoutes, settings.restApi, swaggerConfig).compositeRoute,
        settings.restApi.bindAddress.getAddress.getHostAddress,
        settings.restApi.bindAddress.getPort
      )
    }

  system.actorOf(Props[Zombie], "zombie")
  def forceStopApplication(code: Int = 0, errorMessage: String): Nothing = {
    logger.error(errorMessage)
    system.registerOnTermination {
      println("Actor system is terminated")
    }
    Await.ready(system.terminate(), 1 minute)
    sys.exit(code)
  }

  def commonSupervisorStrategy: OneForOneStrategy =
    OneForOneStrategy(maxNrOfRetries = 5, withinTimeRange = 60 seconds) {
      case _ => Restart
    }
}
