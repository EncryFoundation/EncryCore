package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.api.http.DataHolderForApi.{GetReaders, Readers}
import encry.settings.RESTApiSettings
import encry.view.state.{StateMode, UtxoStateReader}
import io.circe.Json
import io.circe.syntax._
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import scala.concurrent.Future

case class StateInfoApiRoute(dataHolder: ActorRef,
                             restApiSettings: RESTApiSettings,
                             stateMode: StateMode)
                            (implicit val context: ActorRefFactory) extends EncryBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("state") { getBoxByIdR }

  override val settings: RESTApiSettings = restApiSettings

  private def getState: Future[UtxoStateReader] = (dataHolder ? GetReaders).mapTo[Readers].map(_.s.get)

  private def getBoxById(id: ADKey): Future[Option[Json]] = getState.map(_.boxById(id).map(_.asJson))

  def getBoxByIdR: Route = (boxId & get) { key =>
    getBoxById(key).okJson()
  }
}