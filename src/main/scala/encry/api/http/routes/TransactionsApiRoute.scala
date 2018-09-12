package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.EncryProposition
import encry.view.EncryViewReadersHolder.{GetReaders, Readers}
import encry.view.mempool.EncryMempoolReader
import encry.view.state.StateMode
import io.circe.Json
import io.circe.syntax._
import encry.view.EncryNodeViewHolder.ReceivableMessages.{LocallyGeneratedTransaction, Test}
import encry.settings.RESTApiSettings
import scala.concurrent.Future

case class TransactionsApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef,
                                restApiSettings: RESTApiSettings, stateMode: StateMode)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("transactions") {
    getUnconfirmedTransactionsR ~ defaultTransferTransactionR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getMempool: Future[EncryMempoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m.get)

  private def getUnconfirmedTransactions(limit: Int, offset: Int = 0): Future[Json] = getMempool.map {
    _.unconfirmed.values.slice(offset, offset + limit)
  }.map(_.map(_.asJson).asJson)

  def defaultTransferTransactionR: Route = path("send") {
    post(entity(as[List[Transaction]]) {
      txs => complete { nodeViewActorRef ! Test[EncryProposition, Transaction](txs)
          StatusCodes.OK
        }
    })
  }

  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & paging) { (offset, limit) =>
    getUnconfirmedTransactions(limit, offset).okJson()
  }
}
