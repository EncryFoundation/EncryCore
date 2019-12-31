package encry.cli.commands.history

import akka.actor.ActorRef
import akka.util.Timeout
import encry.cli.{ Ast, Response }
import encry.cli.commands.Command
import encry.settings.EncryAppSettings

import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern._
import encry.api.http.DataHolderForApi.{ GetDataFromHistory, GetFullBlockByIdCommand }
import encry.utils.NetworkTimeProvider
import encry.view.history.History
import io.circe.syntax._
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.concurrent.Future

object GetHeaderById extends Command {

  /**
    * Command "history getHeaderById -modifier=<addr[String]>"
    * Example "history getHeaderById -modifier='5a770264e69cb097049d38196d2d0213a69604130e4bb822aaaa931bd8859ea5'"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)

    val num = args.requireArg[Ast.Str]("modifier").s

    (dataHolder ? GetFullBlockByIdCommand(Left(num)))
      .mapTo[Option[Block]]
      .map(x => Some(Response(x.map(_.header).asJson.toString())))
  }

}
