package encry.api.routes

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import io.swagger.annotations.Api
import scorex.core.settings.RESTApiSettings

@Path("/mining")
@Api(value = "/mining", produces = "application/json")
case class MiningApiRoute(miner: ActorRef, override val settings: RESTApiSettings)
                         (implicit val context: ActorRefFactory) extends EncryBaseApiRoute {
  override val route: Route = pathPrefix("mining") {
    concat()
  }
//TODO: after miner impl uncom
//  @Path("/status")
//  @ApiOperation(value = "Current state of node miner", httpMethod = "GET")
//  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with current state")))
//  def status: Route = path("status") {
//    get(toJsonResponse((miner ? MiningStatusRequest).mapTo[MiningStatusResponse].map(r =>
//      SuccessApiResponse(r.json))))
//  }
}
