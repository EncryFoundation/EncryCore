package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import encry.api.http.DataHolderForApi._
import encry.settings.RESTApiSettings

case class NodeRoute(dataHolder: ActorRef, settings: RESTApiSettings)(implicit val context: ActorRefFactory)
    extends EncryBaseApiRoute {

  override def route: Route = pathPrefix("node") {
    WebRoute.authRoute(nodeStartMiningR ~ nodeStopMiningR ~ nodeShutdownR, settings)
  }

  def nodeStartMiningR: Route = (path("startMining") & get) {
    dataHolder ! StartMinerApiMessage
    withCors(complete(StatusCodes.OK))
  }

  def nodeStopMiningR: Route = (path("stopMining") & get) {
    dataHolder ! StopMinerApiMessage
    withCors(complete(StatusCodes.OK))
  }

  def nodeShutdownR: Route = (path("shutdown") & get) {
    dataHolder ! ShutdownNode
    withCors(complete(StatusCodes.OK))
  }

}
