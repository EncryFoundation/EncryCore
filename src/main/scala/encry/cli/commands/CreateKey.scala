package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.EncryApp._
import encry.api.http.DataHolderForApi.GetViewCreateKey
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.crypto.PrivateKey25519
import scala.concurrent.Future

object CreateKey extends Command {

  /**
    * Command "wallet createKey"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ? GetViewCreateKey).mapTo[PrivateKey25519]
    Future.successful(Some(Response("Key was created")))
  }
}
