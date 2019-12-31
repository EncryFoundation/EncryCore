package encry.cli.commands.history

import akka.actor.ActorRef
import encry.api.http.DataHolderForApi.GetMinerStatus
import encry.cli.Response
import encry.cli.commands.Command
import encry.local.miner.Miner.MinerStatus
import encry.settings.EncryAppSettings
import akka.pattern._
import akka.util.Timeout
import encry.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import io.circe.syntax._

object GetCandidate extends Command {

  /**
    * Command "history getCandidate"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ? GetMinerStatus).mapTo[MinerStatus].map(x => Some(Response(x.asJson.toString())))
  }
}
