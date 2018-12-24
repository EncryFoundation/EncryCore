package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.api.http.routes.WalletInfoApiRoute.{AssetBoxResponse, DataBoxResponse, EncryBaseBoxResponse, TokenIssuingBoxResponse, WalletInfoResponse}
import encry.modifiers.state.box.{AssetBox, DataBox, EncryBaseBox, TokenIssuingBox}
import encry.settings.RESTApiSettings
import encry.utils.Logging
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.EncryHistory
import encry.view.mempool.Mempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import io.circe.{Encoder, Json}
import io.circe.syntax._
import io.circe.generic.auto._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.media.{ArraySchema, Content, Schema}
import io.swagger.v3.oas.annotations.responses.ApiResponse
import javax.ws.rs.{GET, Path}
import org.encryfoundation.common.Algos

import scala.annotation.meta.field
import scala.concurrent.Future

@Path("/wallet")
case class WalletInfoApiRoute(nodeViewActorRef: ActorRef,
                              restApiSettings: RESTApiSettings)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport with Logging {

  override val route: Route = pathPrefix("wallet") { infoR ~ getUtxosR }

  override val settings: RESTApiSettings = restApiSettings

  private def getWallet: Future[EncryWallet] = (nodeViewActorRef ?
    GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, Mempool, EncryWallet](_.vault))
    .mapTo[EncryWallet]

  @GET
  @Path("/info")
  @Operation(summary = "Return wallet info",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Wallet info response",
        content = Array(new Content(mediaType = "application/json",
          schema = new Schema(implementation = classOf[WalletInfoResponse])))),
      new ApiResponse(responseCode = "500", description = "Internal server error"))
  )
  def infoR: Route = (path("info") & get) {
    getWallet
      .map { w =>
        Map(
          "balances" -> w.getBalances.map(i => Algos.encode(i._1) -> i._2.toString).toMap.asJson,
          "utxosQty" -> w.walletStorage.allBoxes.length.asJson
        ).asJson
      }
      .okJson()
  }

  @GET
  @Path("/utxos")
  @Operation(summary = "Return wallet utxos",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Wallet utxos response",
        content = Array(new Content(mediaType = "application/json",
          array = new ArraySchema(schema =
          new Schema(allOf = Array(classOf[AssetBoxResponse], classOf[DataBoxResponse], classOf[TokenIssuingBoxResponse])))))),
      new ApiResponse(responseCode = "500", description = "Internal server error"))
  )
  def getUtxosR: Route = (path("utxos") & get) {
    getWallet
      .map { _.walletStorage.allBoxes.map(EncryBaseBoxResponse(_)).asJson }
      .okJson()
  }

}

object WalletInfoApiRoute {
  @ApiModel
  case class WalletInfoResponse(
                                 @(ApiModelProperty @field)(value = "current balances") balances: Map[String, String],
                                 @(ApiModelProperty @field)(value = "number of unspent transactions outputs") utxosQty: Int
                               )

  @ApiModel(value = "Base class for encry box", subTypes = Array(classOf[AssetBoxResponse], classOf[DataBoxResponse], classOf[TokenIssuingBoxResponse]))
  sealed trait EncryBaseBoxResponse {
    val typeId: Byte
    val proposition: PropositionResponse
    val id: String
    val nonce: Long
  }

  @ApiModel
  case class PropositionResponse(@(ApiModelProperty @field)(value = "contract hash") contractHash: String)

  @ApiModel(value = "Asset box")
  case class AssetBoxResponse(
                               @(ApiModelProperty @field)(value = "box type, 1 – AssetBox/TokenIssuingBox, 4 – DataBox") typeId: Byte,
                               @(ApiModelProperty @field)(value = "proposition") proposition: PropositionResponse,
                               @(ApiModelProperty @field)(value = "encoded id") id: String,
                               @(ApiModelProperty @field)(value = "nonce") nonce: Long,
                               @(ApiModelProperty @field)(value = "value") value: Long,
                               @(ApiModelProperty @field)(value = "encoded token id", required = false) tokenId: Option[String]
                             ) extends EncryBaseBoxResponse

  @ApiModel(value = "Token issuing box")
  case class TokenIssuingBoxResponse (
                                       @(ApiModelProperty @field)(value = "box type, 1 – AssetBox/TokenIssuingBox, 4 – DataBox") typeId: Byte,
                                       @(ApiModelProperty @field)(value = "proposition") proposition: PropositionResponse,
                                       @(ApiModelProperty @field)(value = "encoded id") id: String,
                                       @(ApiModelProperty @field)(value = "nonce") nonce: Long
                                     ) extends EncryBaseBoxResponse

  @ApiModel(value = "Data box")
  case class DataBoxResponse(
                              @(ApiModelProperty @field)(value = "box type, 1 – AssetBox/TokenIssuingBox, 4 – DataBox") typeId: Byte,
                              @(ApiModelProperty @field)(value = "proposition") proposition: PropositionResponse,
                              @(ApiModelProperty @field)(value = "encoded id") id: String,
                              @(ApiModelProperty @field)(value = "nonce") nonce: Long,
                              @(ApiModelProperty @field)(value = "encoded data") data: String
                            ) extends EncryBaseBoxResponse

  object EncryBaseBoxResponse {
    def apply(box: EncryBaseBox): EncryBaseBoxResponse = box match {
      case ab: AssetBox =>
        AssetBoxResponse(ab.typeId,
          PropositionResponse(Algos.encode(ab.proposition.contractHash)),
          Algos.encode(ab.id),
          ab.nonce,
          ab.amount,
          ab.tokenIdOpt.map(id => Algos.encode(id))
        )
      case tib: TokenIssuingBox =>
        TokenIssuingBoxResponse(tib.typeId, PropositionResponse(Algos.encode(tib.proposition.contractHash)), Algos.encode(tib.id), tib.nonce)
      case db: DataBox =>
        DataBoxResponse(db.typeId, PropositionResponse(Algos.encode(db.proposition.contractHash)), Algos.encode(db.id), db.nonce, Algos.encode(db.data))
    }
  }
}
