package encry.cli.commands.history

import akka.actor.ActorRef
import encry.api.http.DataHolderForApi.{ GetDataFromHistory, GetLastHeadersHelper }
import encry.cli.{ Ast, Response }
import encry.cli.commands.Command
import encry.settings.EncryAppSettings
import akka.pattern._
import akka.util.Timeout
import encry.utils.NetworkTimeProvider
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import org.encryfoundation.common.modifiers.history.Header
import io.circe.syntax._

object GetLastHeaders extends Command {

  /**
    * Command "history getLastHeaders -count=<height[Num]>"
    * Example "history getLastHeaders -count=5"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    val num: Long                 = args.requireArg[Ast.Num]("count").i
    (dataHolder ? GetLastHeadersHelper(num.toInt))
      .mapTo[IndexedSeq[Header]]
      .map(x => Some(Response(x.asJson.toString())))
  }
}
