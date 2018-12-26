package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.api.http.routes.TransactionsApiRoute.TransactionResponse
import encry.api.http.routes.WalletInfoApiRoute.EncryBaseBoxResponse
import encry.modifiers.mempool.Transaction
import encry.modifiers.mempool.directive._
import encry.modifiers.state.box.EncryProposition
import encry.view.ReadersHolder.{GetReaders, Readers}
import encry.view.mempool.MempoolReader
import encry.view.state.StateMode
import io.circe.{Encoder, Json}
import io.circe.syntax._
import io.circe.generic.auto._
import encry.view.EncryNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import encry.settings.RESTApiSettings
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import io.swagger.v3.oas.annotations.{Operation, Parameter}
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.{ArraySchema, Content, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.responses.ApiResponse

import scala.concurrent.Future
import javax.ws.rs.{GET, POST, Path}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.transaction.{Input, InputSerializer, Proof}
import org.encryfoundation.prismlang.codec.PCodec

import scala.annotation.meta.field

@Path("/transactions")
case class TransactionsApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef,
                                restApiSettings: RESTApiSettings, stateMode: StateMode)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("transactions") {
    getUnconfirmedTransactionsR ~ defaultTransferTransactionR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getMempool: Future[MempoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m.get)

  private def getUnconfirmedTransactions(limit: Int, offset: Int = 0): Future[Json] = getMempool.map {
    _.unconfirmed.values.slice(offset, offset + limit)
  }.map(_.map(TransactionResponse(_)).asJson)

  @POST
  @Path("/send")
  @Operation(summary = "Send unconfirmed transactions",
    requestBody = new RequestBody(content = Array(new Content(schema = new Schema(implementation = classOf[TransactionResponse])))),
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Transaction sent to NodeViewHolder")
    )
  )
  def defaultTransferTransactionR: Route = path("send") {
    post(entity(as[Transaction]) {
      tx =>
        complete {
          nodeViewActorRef ! LocallyGeneratedTransaction[EncryProposition, Transaction](tx)
          StatusCodes.OK
        }
    })
  }

  @GET
  @Path("/unconfirmed")
  @Operation(summary = "Return unconfirmed transactions",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Unconfirmed transactions response",
        content = Array(new Content(mediaType = "application/json",
          array = new ArraySchema(schema = new Schema(implementation = classOf[TransactionResponse]))))),
      new ApiResponse(responseCode = "500", description = "Internal server error")),
    parameters = Array(
      new Parameter(name = "offset", required = false, schema = new Schema(implementation = classOf[Int], defaultValue = "0"), in = ParameterIn.QUERY),
      new Parameter(name = "limit", required = false, schema = new Schema(implementation = classOf[Int], defaultValue = "50"), in = ParameterIn.QUERY)
    )
  )
  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & paging) { (offset, limit) =>
    getUnconfirmedTransactions(limit, offset).okJson()
  }
}

object TransactionsApiRoute {

  @ApiModel(value = "Transaction")
  case class TransactionResponse(@(ApiModelProperty @field)(value = "transaction fee", example = "985") fee: Long,
                                 @(ApiModelProperty @field)(value = "transaction id",
                                   example = "267651125e75738e777f780bf12b3c2756b645c185378bc5e1ce8fe8c63f385f") id: String,
                                 @(ApiModelProperty @field)(value = "transaction timestamp") timestamp: Long,
                                 @(ApiModelProperty @field)(value = "transaction inputs") inputs: Seq[InputResponse],
                                 @(ApiModelProperty @field)(value = "transaction directives") directives: Seq[DirectiveResponse],
                                 @(ApiModelProperty @field)(value = "transaction outputs") outputs: Seq[EncryBaseBoxResponse],
                                 @(ApiModelProperty @field)(value = "transaction proofs", required = false) defaultProofOpt: Option[ProofResponse])

  object TransactionResponse {
    def apply(tx: Transaction): TransactionResponse = TransactionResponse(
      tx.fee,
      Algos.encode(tx.id),
      tx.timestamp,
      tx.inputs.map(InputResponse(_)),
      tx.directives.map(DirectiveResponse(_)),
      tx.newBoxes.toSeq.map(EncryBaseBoxResponse(_)),
      tx.defaultProofOpt.map(ProofResponse(_))
    )
  }

  @ApiModel(value = "Proof")
  case class ProofResponse(
                            @(ApiModelProperty @field)(value = "serialized value",
                              example = "5000000404e4c44696eb0f6bc54e6f4e11f16fc1eda17fdae7c30d5c211639349250ecf1817380b" +
                                "9efb61315b0e402e404e871c5f5862f664513e61f2a9e1be9ae293d0020") serializedValue: String,
                            @(ApiModelProperty @field)(value = "tag", required = false) tag: Option[String])

  object ProofResponse {
    def apply(proof: Proof): ProofResponse = ProofResponse(
      Algos.encode(PCodec.boxedValCodec.encode(proof.value).require.toByteArray),
      proof.tagOpt
    )
  }

  @ApiModel(value = "Transaction input")
  case class InputResponse(
                            @(ApiModelProperty @field)(value = "box id") boxId: String,
                            @(ApiModelProperty @field)(value = "serialized contract") contract: String,
                            @(ApiModelProperty @field)(value = "input proofs") proofs: Seq[ProofResponse])

  object InputResponse {
    def apply(input: Input): InputResponse = InputResponse(
      Algos.encode(input.boxId),
      Algos.encode(InputSerializer.encodeEitherCompiledOrRegular(input.contract)),
      input.proofs.map(ProofResponse(_))
    )
  }

  @ApiModel(value = "Base transaction directive")
  sealed trait DirectiveResponse {
    val typeId: Byte
  }

  object DirectiveResponse {
    def apply(d: Directive): DirectiveResponse = d match {
      case td: TransferDirective => TransferDirectiveResponse(td.typeId, td.address, td.amount, td.tokenIdOpt.map(id => Algos.encode(id)).getOrElse("null"))
      case aid: AssetIssuingDirective => AssetIssuingDirectiveResponse(aid.typeId, Algos.encode(aid.contractHash), aid.amount)
      case sad: ScriptedAssetDirective =>
        ScriptedAssetDirectiveResponse(sad.typeId, Algos.encode(sad.contractHash), sad.amount, sad.tokenIdOpt.map(id => Algos.encode(id)))
      case dad: DataDirective => DataDirectiveResponse(dad.typeId, Algos.encode(dad.contractHash), Algos.encode(dad.data))
    }
  }

  @ApiModel(value = "Transfer directive")
  case class TransferDirectiveResponse(
                                        @(ApiModelProperty @field)(value = "directive type id") typeId: Byte,
                                        @(ApiModelProperty @field)(value = "address")  address: String,
                                        @(ApiModelProperty @field)(value = "amount")  amount: Long,
                                        @(ApiModelProperty @field)(value = "token id")  tokenId: String
                                      ) extends DirectiveResponse

  @ApiModel(value = "Asset issuing directive")
  case class AssetIssuingDirectiveResponse(
                                        @(ApiModelProperty @field)(value = "directive type id") typeId: Byte,
                                        @(ApiModelProperty @field)(value = "contract hash")  contractHash: String,
                                        @(ApiModelProperty @field)(value = "amount")  amount: Long
                                      ) extends DirectiveResponse

  @ApiModel(value = "Scripted asset directive")
  case class ScriptedAssetDirectiveResponse(
                                            @(ApiModelProperty @field)(value = "directive type id") typeId: Byte,
                                            @(ApiModelProperty @field)(value = "contract hash")  contractHash: String,
                                            @(ApiModelProperty @field)(value = "amount")  amount: Long,
                                            @(ApiModelProperty @field)(value = "token id option", required = false)  tokenId: Option[String]
                                          ) extends DirectiveResponse

  @ApiModel(value = "Data directive")
  case class DataDirectiveResponse(
                                    @(ApiModelProperty @field)(value = "directive type id") typeId: Byte,
                                    @(ApiModelProperty @field)(value = "contract hash")  contractHash: String,
                                    @(ApiModelProperty @field)(value = "data")  data: String
                                  ) extends DirectiveResponse

}
