package encry

import java.net.InetAddress
import akka.actor.SupervisorStrategy.Restart
import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.server.ExceptionHandler
import akka.stream.ActorMaterializer
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.routes._
import encry.api.http.{ApiRoute, CompositeHttpService, PeersApiRoute, UtilsApiRoute}
import encry.cli.ConsoleListener
import encry.cli.ConsoleListener.StartListening
import encry.local.explorer.database.DBService
import encry.local.miner.Miner
import encry.local.miner.Miner.StartMining
import encry.network.message._
import encry.network.{PeerManager, _}
import encry.settings.EncryAppSettings
import encry.stats.{StatsSender, Zombie}
import encry.utils.NetworkTimeProvider
import encry.view.{EncryNodeViewHolder, ReadersHolder}
import org.encryfoundation.common.Algos
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

object EncryApp extends App with StrictLogging {

  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  val settings: EncryAppSettings = EncryAppSettings.read
  lazy val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)
  lazy val dbService: DBService = new DBService(settings)
  val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")
  val nodeId: Array[Byte] = Algos.hash(settings.network.nodeName
    .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)).take(5)
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

  val statsSenderOpt: Option[ActorRef] =
    if (settings.influxDB.isDefined) Some(system.actorOf(Props(classOf[StatsSender], settings), "statsSender"))
    else None
  val readersHolder: ActorRef = system.actorOf(Props[ReadersHolder], "readersHolder")
  val networkController: ActorRef = system.actorOf(Props(classOf[NetworkController], settings, peerManager, statsSenderOpt)
    .withDispatcher("network-dispatcher"), "networkController")
  lazy val peerManager: ActorRef = system.actorOf(Props(classOf[PeerManager], settings, timeProvider), "peerManager")
  val nvhProps: Props = EncryNodeViewHolder.props(settings, peerManager, timeProvider, statsSenderOpt, readersHolder, dbService, networkController)
  val nodeViewHolder: ActorRef =
    system.actorOf(nvhProps.withDispatcher("nvh-dispatcher").withMailbox("nvh-mailbox"), "nodeViewHolder")
  readersHolder ! nodeViewHolder
  lazy val miner: ActorRef = system.actorOf(Props(classOf[Miner], settings, nodeViewHolder, timeProvider, statsSenderOpt), "miner")
  if (settings.node.mining) miner ! StartMining
  if (settings.node.useCli) {
    system.actorOf(Props(classOf[ConsoleListener], settings), "cliListener")
    system.actorSelection("/user/cliListener") ! StartListening
  }
  system.actorOf(Props[Zombie], "zombie")

  if (settings.restApi.enabled.getOrElse(false)) {
    import akka.http.scaladsl.model.StatusCodes._
    import akka.http.scaladsl.server.Directives._
    implicit def apiExceptionHandler: ExceptionHandler =
      ExceptionHandler {
        case e: Exception =>
          extractUri { uri =>
            logger.error(s"Request to $uri could not be handled normally due to: $e")
            complete(HttpResponse(InternalServerError, entity = "Internal server error"))
          }
      }

    val apiRoutes: Seq[ApiRoute] = Seq(
      UtilsApiRoute(settings.restApi),
      PeersApiRoute(peerManager, networkController, settings.restApi),
      InfoApiRoute(readersHolder, miner, peerManager, settings, nodeId, timeProvider),
      HistoryApiRoute(readersHolder, miner, settings, nodeId, settings.node.stateMode),
      TransactionsApiRoute(readersHolder, nodeViewHolder, settings.restApi, settings.node.stateMode),
      StateInfoApiRoute(readersHolder, nodeViewHolder, settings.restApi, settings.node.stateMode),
      WalletInfoApiRoute(nodeViewHolder, settings.restApi)
    )
    Http().bindAndHandle(
      CompositeHttpService(system, apiRoutes, settings.restApi, swaggerConfig).compositeRoute,
      settings.restApi.bindAddress.getAddress.getHostAddress,
      settings.restApi.bindAddress.getPort)
  }

  def forceStopApplication(code: Int = 0): Nothing = {
    system.registerOnTermination {
      println("Actor system is terminated.")
    }
    Await.ready(system.terminate(), 1 minute)
    sys.exit(code)
  }

  def commonSupervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = 5,
    withinTimeRange = 60 seconds) {
    case _ => Restart
  }

}