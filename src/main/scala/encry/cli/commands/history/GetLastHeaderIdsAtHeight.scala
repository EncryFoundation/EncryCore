package encry.cli.commands.history

import akka.actor.ActorRef
import encry.api.http.DataHolderForApi.GetDataFromHistory
import encry.cli.{Ast, Response}
import encry.cli.commands.Command
import encry.settings.EncryAppSettings
import encry.view.history.History
import akka.pattern._
import akka.util.Timeout
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.utils.Algos

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import io.circe.syntax._

object GetLastHeaderIdsAtHeight extends Command {
  override def execute(args: Command.Args, settings: EncryAppSettings, dataHolder: ActorRef,nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)

    def getHistory: Future[History] = (dataHolder ? GetDataFromHistory).mapTo[History]

    def getHeaderIdsAtHeight(n: Int): Future[Unit] = getHistory.map {
      x => println(x.headerIdsAtHeight(n).map(Algos.encode))
    }

    val num = args.requireArg[Ast.Num]("at").i
    getHeaderIdsAtHeight(num.toInt)
    Future(None)
  }
}
