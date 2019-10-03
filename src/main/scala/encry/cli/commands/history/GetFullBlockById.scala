package encry.cli.commands.history

import akka.actor.ActorRef
import akka.util.Timeout
import encry.api.http.DataHolderForApi.GetDataFromHistory
import encry.cli.{Ast, Response}
import encry.cli.commands.Command
import encry.settings.EncryAppSettings
import encry.view.history.History
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import akka.pattern._
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.history.Block
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import io.circe.syntax._

object GetFullBlockById extends Command{
  override def execute(args: Command.Args, settings: EncryAppSettings, dataHolder: ActorRef,nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    def getHistory: Future[History] = (dataHolder ? GetDataFromHistory).mapTo[History]

    def getFullBlockByHeaderId(headerId: String): Future[Option[Block]] = getHistory.map { history =>
      Algos.decode(headerId).toOption
        .flatMap(decoded => history.getHeaderById(ModifierId @@ decoded))
        .flatMap(history.getBlockByHeader)
    }
    val num = args.requireArg[Ast.Str]("modifier").s
    getFullBlockByHeaderId(num).map(_.map(x => println(x.asJson.toString())))
    Future(None)
  }
}
//history getFullBlock -modifier='8b2ccc8718e9a244dc92cf962bfe1279318b96a134dd5462a59079d74144c0f7'