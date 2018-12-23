package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.utils.CoreTaggedTypes.ModifierId
import encry.local.miner.Miner.{GetMinerStatus, MinerStatus}
import encry.modifiers.history.{Block, Header}
import encry.modifiers.mempool.Transaction
import encry.settings.{EncryAppSettings, RESTApiSettings}
import encry.view.ReadersHolder.GetDataFromHistory
import encry.view.history.EncryHistoryReader
import encry.view.state.StateMode
import io.circe.Json
import io.circe.syntax._
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.{Operation, Parameter}
import io.swagger.v3.oas.annotations.media.{ArraySchema, Content, Schema}
import io.swagger.v3.oas.annotations.responses.ApiResponse
import javax.ws.rs.{GET, Path}
import org.encryfoundation.common.Algos

import scala.concurrent.Future

@Path("/history")
case class HistoryApiRoute(readersHolder: ActorRef, miner: ActorRef, appSettings: EncryAppSettings,
                           nodeId: Array[Byte], stateMode: StateMode)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute {

  override val route: Route = pathPrefix("history") {
    getBlocksR ~
      getLastHeadersR ~
      getBlockIdsAtHeightR ~
      getBlockHeaderByHeaderIdR ~
      getBlockTransactionsByHeaderIdR ~
      getFullBlockByHeaderIdR ~
      candidateBlockR
  }

  override val settings: RESTApiSettings = appSettings.restApi

  private def getHistory: Future[EncryHistoryReader] = (readersHolder ? GetDataFromHistory).mapTo[EncryHistoryReader]

  private def getHeaderIdsAtHeight(h: Int): Future[Json] = getHistory.map {
    _.headerIdsAtHeight(h).map(Algos.encode).asJson
  }

  private def getLastHeaders(n: Int): Future[Json] = getHistory.map {
    _.lastHeaders(n).headers.map(_.asJson).asJson
  }

  private def getHeaderIds(offset: Int, limit: Int): Future[Json] =
    getHistory.map {
      _.getHeaderIds(limit, offset).map(Algos.encode).asJson
    }

  private def getFullBlockByHeaderId(headerId: ModifierId): Future[Option[Block]] = getHistory.map { history =>
    history.typedModifierById[Header](headerId).flatMap(history.getBlock)
  }

  @GET
  @Path("/")
  @Operation(summary = "Return ids of {limit} headers starting from {offset}",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Headers ids response",
        content = Array(new Content(array = new ArraySchema(schema = new Schema(implementation = classOf[String]))))),
      new ApiResponse(responseCode = "500", description = "Internal server error")),
    parameters = Array(
      new Parameter(name = "offset", required = false, schema = new Schema(implementation = classOf[Int], defaultValue = "0"), in = ParameterIn.QUERY),
      new Parameter(name = "limit", required = false, schema = new Schema(implementation = classOf[Int], defaultValue = "50"), in = ParameterIn.QUERY)
    )
  )
  def getBlocksR: Route = (pathEndOrSingleSlash & get & paging) { (offset, limit) => getHeaderIds(offset, limit).okJson() }

  @GET
  @Path("/lastHeaders/{quantity}")
  @Operation(summary = "Return {quantity} last headers",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Headers response",
        content = Array(new Content(array = new ArraySchema(schema = new Schema(implementation = classOf[Header]))))),
      new ApiResponse(responseCode = "500", description = "Internal server error")),
    parameters = Array(
      new Parameter(name = "quantity", required = true, schema = new Schema(implementation = classOf[Int]), in = ParameterIn.PATH),
    )
  )
  def getLastHeadersR: Route = (pathPrefix("lastHeaders" / IntNumber) & get) { qty => getLastHeaders(qty).okJson() }

  @GET
  @Path("/at/{height}")
  @Operation(summary = "Return ids of all headers at {height}",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Headers ids response",
        content = Array(new Content(array = new ArraySchema(schema = new Schema(implementation = classOf[String]))))),
      new ApiResponse(responseCode = "500", description = "Internal server error")),
    parameters = Array(
      new Parameter(name = "height", required = true, schema = new Schema(implementation = classOf[Int]), in = ParameterIn.PATH),
    )
  )
  def getBlockIdsAtHeightR: Route = (pathPrefix("at" / IntNumber) & get) { height => getHeaderIdsAtHeight(height).okJson() }

  @GET
  @Path("/{modifierId}/header")
  @Operation(summary = "Return header with given {modifierId}",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Header response",
        content = Array(new Content(schema = new Schema(implementation = classOf[String])))),
      new ApiResponse(responseCode = "500", description = "Internal server error")),
    parameters = Array(
      new Parameter(name = "modifierId",
        required = true,
        schema = new Schema(implementation = classOf[String]),
        in = ParameterIn.PATH,
        description = "Base58-encoded 32 byte modifier id"
      ),
    )
  )
  def getBlockHeaderByHeaderIdR: Route = (modifierId & pathPrefix("header") & get) { id =>
    getFullBlockByHeaderId(id).map(_.map(_.header.asJson)).okJson()
  }

  @GET
  @Path("/{modifierId}/transactions")
  @Operation(summary = "Return list of transactions of block with given {modifierId}",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Transaction response",
        content = Array(new Content(array = new ArraySchema(schema = new Schema(implementation = classOf[String]))))),
      new ApiResponse(responseCode = "500", description = "Internal server error")),
    parameters = Array(
      new Parameter(name = "modifierId", required = true, schema = new Schema(implementation = classOf[String]), in = ParameterIn.PATH),
    )
  )
  def getBlockTransactionsByHeaderIdR: Route = (modifierId & pathPrefix("transactions") & get) { id =>
    getFullBlockByHeaderId(id).map(_.map(_.transactions.asJson)).okJson()
  }

  @GET
  @Path("/candidateBlock")
  @Operation(summary = "Return current candidate block",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Candidate block response",
        content = Array(new Content(schema = new Schema(implementation = classOf[Transaction])))),
      new ApiResponse(responseCode = "500", description = "Internal server error"))
  )
  def candidateBlockR: Route = (path("candidateBlock") & pathEndOrSingleSlash & get) {
    (miner ? GetMinerStatus).mapTo[MinerStatus].map(_.json).okJson()
  }

  @GET
  @Path("/{modifierId}")
  @Operation(summary = "Return full block with given {modifierId}",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Block response",
        content = Array(new Content(schema = new Schema(implementation = classOf[Block])))),
      new ApiResponse(responseCode = "500", description = "Internal server error")),
    parameters = Array(
      new Parameter(name = "modifierId", required = true, schema = new Schema(implementation = classOf[String]), in = ParameterIn.PATH),
    )
  )
  def getFullBlockByHeaderIdR: Route = (modifierId & get) { id =>
    getFullBlockByHeaderId(id).map(_.map(_.asJson)).okJson()
  }
}
