package encry.cli.commands.history

import akka.actor.ActorRef
import akka.util.Timeout
import encry.cli.{ Ast, Response }
import encry.cli.commands.Command
import encry.settings.EncryAppSettings
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern._
import encry.api.http.DataHolderForApi.GetDataFromHistory
import encry.utils.NetworkTimeProvider
import encry.view.history.History
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.concurrent.Future

object GetTxById extends Command {
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)

    def getFullBlockByHeaderId(headerId: String): Future[Option[Block]] =
      (dataHolder ?
        GetDataFromHistory).mapTo[History].map { history =>
        Algos
          .decode(headerId)
          .toOption
          .flatMap(decoded => history.getHeaderById(ModifierId @@ decoded))
          .flatMap(history.getBlockByHeader)
      }
    val mod = args.requireArg[Ast.Str]("modifier").s
    getFullBlockByHeaderId(mod).map(_.flatMap(x => Some(Response(x.payload.txs.asJson.toString))))
  }

}
