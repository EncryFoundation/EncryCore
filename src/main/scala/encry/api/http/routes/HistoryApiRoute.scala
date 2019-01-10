package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.api.http.routes.HistoryApiRoute._
import encry.api.http.routes.TransactionsApiRoute.TransactionResponse
import encry.consensus.CandidateBlock
import encry.utils.CoreTaggedTypes.ModifierId
import encry.local.miner.Miner.{GetMinerStatus, MinerStatus}
import encry.modifiers.history.{Block, Header}
import encry.settings.{EncryAppSettings, RESTApiSettings}
import encry.view.ReadersHolder.GetDataFromHistory
import encry.view.history.EncryHistoryReader
import encry.view.state.StateMode
import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.{Operation, Parameter}
import io.swagger.v3.oas.annotations.media.{ArraySchema, Content, Schema}
import io.swagger.v3.oas.annotations.responses.ApiResponse
import javax.ws.rs.{GET, Path}
import org.encryfoundation.common.Algos

import scala.annotation.meta.field
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
    _.lastHeaders(n).headers.map(HeaderResponse(_).asJson).asJson
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
        content = Array(new Content(mediaType = "text",
          array = new ArraySchema(schema = new Schema(implementation = classOf[String]))))),
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
        content = Array(new Content(mediaType = "application/json",
          array = new ArraySchema(schema = new Schema(implementation = classOf[HeaderResponse]))))),
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
        content = Array(new Content(mediaType = "text",array = new ArraySchema(schema = new Schema(implementation = classOf[String]))))),
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
        content = Array(new Content(mediaType = "application/json",
          schema = new Schema(implementation = classOf[HeaderResponse])))),
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
    getFullBlockByHeaderId(id).map(_.map(block => HeaderResponse(block.header)).asJson).okJson()
  }

  @GET
  @Path("/{modifierId}/transactions")
  @Operation(summary = "Return list of transactions of block with given {modifierId}",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Transaction response",
        content = Array(new Content(mediaType = "application/json",
          array = new ArraySchema(schema = new Schema(implementation = classOf[TransactionResponse]))))),
      new ApiResponse(responseCode = "500", description = "Internal server error")),
    parameters = Array(
      new Parameter(name = "modifierId", required = true, schema = new Schema(implementation = classOf[String]), in = ParameterIn.PATH),
    )
  )
  def getBlockTransactionsByHeaderIdR: Route = (modifierId & pathPrefix("transactions") & get) { id =>
    getFullBlockByHeaderId(id).map(_.map(_.transactions.map(TransactionResponse(_)).asJson)).okJson()
  }

  @GET
  @Path("/candidateBlock")
  @Operation(summary = "Return current candidate block",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Candidate block response",
        content = Array(new Content(mediaType = "application/json",
          schema = new Schema(implementation = classOf[MinerStatusResponse])))),
      new ApiResponse(responseCode = "500", description = "Internal server error"))
  )
  def candidateBlockR: Route = (path("candidateBlock") & pathEndOrSingleSlash & get) {
    (miner ? GetMinerStatus)
      .mapTo[MinerStatus]
      .map(ms => MinerStatusResponse(ms.isMining, ms.candidateBlock.map(CandidateBlockResponse(_))).asJson)
      .okJson()
  }

  @GET
  @Path("/{modifierId}")
  @Operation(summary = "Return full block with given {modifierId}",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Block response",
        content = Array(new Content(mediaType = "application/json",
          schema = new Schema(implementation = classOf[BlockResponse])))),
      new ApiResponse(responseCode = "500", description = "Internal server error")),
    parameters = Array(
      new Parameter(name = "modifierId", required = true, schema = new Schema(implementation = classOf[String]), in = ParameterIn.PATH),
    )
  )
  def getFullBlockByHeaderIdR: Route = (modifierId & get) { id =>
    getFullBlockByHeaderId(id)
      .map(_.map { b =>
        BlockResponse(
          HeaderResponse(b.header),
          PayloadResponse(Algos.encode(b.header.id), b.payload.transactions.map(TransactionResponse(_))),
          b.adProofsOpt.map(proofs => ADProofsResponse(Algos.encode(b.header.id), Algos.encode(proofs.proofBytes), Algos.encode(proofs.digest)))
        ).asJson
      }).okJson()
  }
}

object HistoryApiRoute {

  @ApiModel(value = "Block")
  case class BlockResponse(@(ApiModelProperty @field)(value = "header") header: HeaderResponse,
                           @(ApiModelProperty @field)(value = "payload") payload: PayloadResponse,
                           @(ApiModelProperty @field)(value = "ad proofs", required = false) adProofs: Option[ADProofsResponse])

  @ApiModel(value = "AD proofs")
  case class ADProofsResponse(@(ApiModelProperty @field)(value = "header id") headerId: String,
                              @(ApiModelProperty @field)(value = "serialized proof bytes") proof: String,
                              @(ApiModelProperty @field)(value = "serialized digest") digest: String)

  @ApiModel(value = "Block payload")
  case class PayloadResponse(@(ApiModelProperty @field)(value = "header id") headerId: String,
                             @(ApiModelProperty @field)(value = "block transactions") transactions: Seq[TransactionResponse])

  @ApiModel(value = "Block header")
  case class HeaderResponse(@(ApiModelProperty @field)(value = "header id") id: String,
                            @(ApiModelProperty @field)(value = "version") version: Byte,
                            @(ApiModelProperty @field)(value = "parent id") parentId: String,
                            @(ApiModelProperty @field)(value = "encoded ad proofs root") adProofsRoot: String,
                            @(ApiModelProperty @field)(value = "payload id") payloadId: String,
                            @(ApiModelProperty @field)(value = "state root") stateRoot: String,
                            @(ApiModelProperty @field)(value = "transactions root") txRoot: String,
                            @(ApiModelProperty @field)(value = "nonce") nonce: Long,
                            @(ApiModelProperty @field)(value = "timestamp") timestamp: Long,
                            @(ApiModelProperty @field)(value = "height") height: Int,
                            @(ApiModelProperty @field)(value = "difficulty") difficulty: Long,
                            @(ApiModelProperty @field)(value = "equihash solution") equihashSolution: EquihashSolutionResponse)

  object HeaderResponse {
    def apply(h: Header): HeaderResponse = HeaderResponse(
      Algos.encode(h.id),
      h.version,
      Algos.encode(h.parentId),
      Algos.encode(h.adProofsRoot),
      Algos.encode(h.payloadId),
      Algos.encode(h.stateRoot),
      Algos.encode(h.transactionsRoot),
      h.nonce,
      h.timestamp,
      h.height,
      h.difficulty.toLong,
      EquihashSolutionResponse(h.equihashSolution.ints)
    )
  }

  @ApiModel(value = "Equihash solution")
  case class EquihashSolutionResponse(@(ApiModelProperty @field)(value = "solution value") ints: Seq[Int])

  @ApiModel(value = "Miner status")
  case class MinerStatusResponse(@(ApiModelProperty @field)(value = "indicator if node is mining") isMining: Boolean,
                                 @(ApiModelProperty @field)(value = "current candidate block") candidateBlock: Option[CandidateBlockResponse])

  @ApiModel(value = "Candidate block")
  case class CandidateBlockResponse(@(ApiModelProperty @field)(value = "parent block header") parentOpt: Option[HeaderResponse],
                                    @(ApiModelProperty @field)(value = "serialized AD proofs") adProofBytes: Array[Byte],
                                    @(ApiModelProperty @field)(value = "state root") stateRoot: String,
                                    @(ApiModelProperty @field)(value = "version") version: Byte,
                                    @(ApiModelProperty @field)(value = "transactions") transactions: Seq[TransactionResponse],
                                    @(ApiModelProperty @field)(value = "timestamp") timestamp: Long,
                                    @(ApiModelProperty @field)(value = "difficulty") difficulty: BigInt)

  object CandidateBlockResponse {
    def apply(cb: CandidateBlock): CandidateBlockResponse = CandidateBlockResponse(
      cb.parentOpt.map(HeaderResponse(_)),
      cb.adProofBytes,
      Algos.encode(cb.stateRoot),
      cb.version,
      cb.transactions.map(TransactionResponse(_)),
      cb.timestamp,
      cb.difficulty
    )
  }


}
