package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.api.models.PaymentTransactionModel
import encry.modifiers.mempool.PaymentTransaction
import encry.view.EncryViewReadersHolder.{GetReaders, Readers}
import encry.view.history.EncryHistoryReader
import encry.view.mempool.EncryMempoolReader
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.ModifierId
import scorex.core.settings.RESTApiSettings
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.encode.Base16
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

import scala.concurrent.Future

case class TransactionsApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef,
                                restApiSettings: RESTApiSettings, digest: Boolean)
                               (implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  implicit val paymentTxCodec: RootJsonFormat[PaymentTransactionModel] = jsonFormat6(PaymentTransactionModel)

  override val route: Route = pathPrefix("transactions") {
    getUnconfirmedTransactionsR ~ sendTransactionR ~ getTransactionByIdR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getHistory: Future[EncryHistoryReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.h.get)

  private def getMemPool: Future[EncryMempoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m.get)

  private def getTransactionById(id: ModifierId): Future[Option[Json]] = getHistory.map {
    _.modifierById(id)
  }.map(_.map { modifier => ??? })

  private def getUnconfirmedTransactions(limit: Int): Future[Json] = getMemPool.map {
    _.take(limit).toSeq
  }.map(_.map(_.json).asJson)

  def sendTransactionR: Route = (path("transfer") & post & entity(as[PaymentTransactionModel])) { model =>
    nodeViewActorRef ! LocallyGeneratedTransaction[Proposition, PaymentTransaction](model.toBaseObj)
    complete(StatusCodes.OK)
  }

  def getTransactionByIdR: Route = (path(Segment) & get) { id =>
    getTransactionById(ModifierId @@ Base16.decode(id)).okJson()
  }

  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & paging) { (offset, limit) =>
    getUnconfirmedTransactions(limit).okJson()
  }
}
