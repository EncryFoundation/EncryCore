package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.api.http.DataHolderForApi.GetViewGetBalance
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.state.box.Box.Amount

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object GetBalance extends Command {

  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ? GetViewGetBalance).mapTo[Map[String, Amount]].foreach(s => println(s.keys + " : " + s.values))
    Future(None)
  }
}
