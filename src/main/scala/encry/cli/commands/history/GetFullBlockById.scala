package encry.cli.commands.history

import akka.actor.ActorRef
import akka.util.Timeout
import encry.api.http.DataHolderForApi.GetFullBlockByIdCommand
import encry.cli.{Ast, Response}
import encry.cli.commands.Command
import encry.settings.EncryAppSettings
import akka.pattern._
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.history.Block
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import io.circe.syntax._

object GetFullBlockById extends Command {

  /**
    * Command "history getFullBlock -modifier=<addr[String]>"
    * Example "history getFullBlock -modifier='5a770264e69cb097049d38196d2d0213a69604130e4bb822aaaa931bd8859ea5'"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)

    val mod = args.requireArg[Ast.Str]("modifier").s

    (dataHolder ? GetFullBlockByIdCommand(Left(mod))).mapTo[Option[Block]].map(x => Some(Response(x.asJson.toString())))
  }
}