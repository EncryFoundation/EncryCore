package encry

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{ActorSystem, OneForOneStrategy, Props}
import akka.stream.ActorMaterializer
import com.typesafe.scalalogging.StrictLogging
import encry.local.explorer.database.DBService
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.concurrent.duration._
import scala.language.postfixOps

object EncryApp extends App with StrictLogging {

  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  lazy val settings: EncryAppSettings = EncryAppSettings.read(args.headOption)
  lazy val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)
  lazy val dbService: DBService = DBService()

  system.actorOf(Props(classOf[Starter], system, settings,timeProvider, dbService))

  def forceStopApplication(code: Int = 0): Nothing = {
    system.registerOnTermination { println("Actor system is terminated.") }
    Await.ready(system.terminate(), 1 minute)
    sys.exit(code)
  }

  def commonSupervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = 5,
    withinTimeRange = 60 seconds) {
    case _ => Restart
  }
}