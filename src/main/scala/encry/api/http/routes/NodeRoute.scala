package encry.api.http.routes

import akka.actor.{ ActorRef, ActorRefFactory }
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import encry.api.http.DataHolderForApi._
import encry.settings.{ EncryAppSettings, RESTApiSettings }

case class NodeRoute(dataHolder: ActorRef, appSettings: EncryAppSettings)(implicit val context: ActorRefFactory)
    extends EncryBaseApiRoute {

  override val settings: RESTApiSettings = appSettings.restApi

  override def route: Route = pathPrefix("node") {
    nodeStartMiningR ~ nodeStopMiningR
  }

  def nodeStartMiningR: Route = (path("startMining") & post) {
    dataHolder ! StartMiner
    withCors(complete(StatusCodes.OK))
  }

  def nodeStopMiningR: Route = (path("stopMining") & post) {
    dataHolder ! StopMiner
    withCors(complete(StatusCodes.OK))
  }
}
