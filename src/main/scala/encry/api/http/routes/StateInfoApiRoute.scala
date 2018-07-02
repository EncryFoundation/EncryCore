package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.settings.RESTApiSettings
import encry.view.EncryViewReadersHolder.{GetReaders, Readers}
import encry.view.state.{StateMode, UtxoStateReader}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.authds.ADKey

import scala.concurrent.Future

case class StateInfoApiRoute(readersHolder: ActorRef,
                             nodeViewActorRef: ActorRef,
                             restApiSettings: RESTApiSettings,
                             stateMode: StateMode)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("state") { getBoxByIdR }

  override val settings: RESTApiSettings = restApiSettings

  private def getState: Future[UtxoStateReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.s.get)

  private def getBoxById(id: ADKey): Future[Option[Json]] = getState.map(_.boxById(id).map(_.asJson))

  def getBoxByIdR: Route = (boxId & get) { key =>
    getBoxById(key).okJson()
  }
}
