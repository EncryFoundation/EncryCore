package encry.cli.commands.history

import akka.actor.ActorRef
import encry.api.http.DataHolderForApi.GetLastHeaderIdAtHeightHelper
import encry.cli.{ Ast, Response }
import encry.cli.commands.Command
import encry.settings.EncryAppSettings
import akka.pattern._
import akka.util.Timeout
import encry.utils.NetworkTimeProvider
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import io.circe.syntax._

object GetLastHeaderIdsAtHeight extends Command {

  /**
    * Command "history getLastHeaderIds -at=<height[Num]>"
    * Example "history getLastHeaderIds -at=5"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)

    val num = args.requireArg[Ast.Num]("at").i

    (dataHolder ? GetLastHeaderIdAtHeightHelper(num.toInt))
      .mapTo[Seq[String]]
      .map(s => Some(Response(s.asJson.toString())))

  }
}
