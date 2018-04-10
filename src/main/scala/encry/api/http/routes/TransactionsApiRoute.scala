package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.api.templates.{AddPubKeyInfoTransactionTemplate, DefaultPaymentTransactionTemplate}
import encry.modifiers.mempool.EncryTransaction
import encry.modifiers.state.box.proposition.EncryProposition
import encry.view.EncryViewReadersHolder.{GetReaders, Readers}
import encry.view.history.EncryHistoryReader
import encry.view.mempool.EncryMempoolReader
import encry.view.state.StateMode
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import scorex.core.LocallyGeneratedModifiersMessages.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.settings.RESTApiSettings
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

import scala.concurrent.Future

case class TransactionsApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef,
                                restApiSettings: RESTApiSettings, stateMode: StateMode)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  implicit val defaultPaymentTxCodec: RootJsonFormat[DefaultPaymentTransactionTemplate] =
    jsonFormat8(DefaultPaymentTransactionTemplate)

  implicit val addPubKeyInfoTxCodec: RootJsonFormat[AddPubKeyInfoTransactionTemplate] =
    jsonFormat10(AddPubKeyInfoTransactionTemplate )

  override val route: Route = pathPrefix("transactions") {
    getUnconfirmedTransactionsR ~
      defaultTransferTransactionR ~
      addPubKeyInfoTransactionR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getHistory: Future[EncryHistoryReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.h.get)

  private def getMempool: Future[EncryMempoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m.get)

  private def getUnconfirmedTransactions(limit: Int): Future[Json] = getMempool.map {
    _.take(limit).toSeq
  }.map(_.map(_.asJson).asJson)

  def defaultTransferTransactionR: Route = (path("transfer") & post & entity(as[DefaultPaymentTransactionTemplate])) { model =>
    model.origin.map { ptx =>
      nodeViewActorRef ! LocallyGeneratedTransaction[EncryProposition, EncryTransaction](ptx)
      complete(StatusCodes.OK)
    }.getOrElse(complete(StatusCodes.BadRequest))
  }

  def addPubKeyInfoTransactionR: Route = (path("add-key-info") & post & entity(as[AddPubKeyInfoTransactionTemplate])) { model =>
    model.origin.map { tx =>
      nodeViewActorRef ! LocallyGeneratedTransaction[EncryProposition, EncryTransaction](tx)
      complete(StatusCodes.OK)
    }.getOrElse(complete(StatusCodes.BadRequest))
  }

  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & paging) { (offset, limit) =>
    getUnconfirmedTransactions(limit).okJson()
  }
}
