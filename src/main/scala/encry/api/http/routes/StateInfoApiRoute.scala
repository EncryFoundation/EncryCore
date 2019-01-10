package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.api.http.routes.WalletInfoApiRoute.{AssetBoxResponse, DataBoxResponse, EncryBaseBoxResponse, TokenIssuingBoxResponse}
import encry.settings.RESTApiSettings
import encry.view.ReadersHolder.{GetReaders, Readers}
import encry.view.state.{StateMode, UtxoStateReader}
import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.{Operation, Parameter}
import io.swagger.v3.oas.annotations.media.{Content, Schema}
import io.swagger.v3.oas.annotations.responses.ApiResponse
import javax.ws.rs.{GET, Path}
import org.encryfoundation.common.utils.TaggedTypes.ADKey

import scala.concurrent.Future

@Path("/state")
case class StateInfoApiRoute(readersHolder: ActorRef,
                             nodeViewActorRef: ActorRef,
                             restApiSettings: RESTApiSettings,
                             stateMode: StateMode)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("state") { getBoxByIdR }

  override val settings: RESTApiSettings = restApiSettings

  private def getState: Future[UtxoStateReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.s.get)

  private def getBoxById(id: ADKey): Future[Option[Json]] = getState.map(_.boxById(id).map(EncryBaseBoxResponse(_)).map(_.asJson))

  @GET
  @Path("/{boxId}")
  @Operation(summary = "Return box with given id",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Box response",
        content = Array(new Content(mediaType = "application/json",
          schema = new Schema(anyOf =
            Array(classOf[AssetBoxResponse], classOf[TokenIssuingBoxResponse], classOf[DataBoxResponse]))))),
      new ApiResponse(responseCode = "500", description = "Internal server error")),
    parameters = Array(
      new Parameter(name = "boxId", required = true, schema = new Schema(implementation = classOf[String]), in = ParameterIn.PATH),
    )
  )
  def getBoxByIdR: Route = (boxId & get) { key =>
    getBoxById(key).okJson()
  }
}
