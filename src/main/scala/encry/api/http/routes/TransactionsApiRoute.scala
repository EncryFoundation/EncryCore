package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.settings.RESTApiSettings
import encry.view.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction

case class TransactionsApiRoute(dataHolder: ActorRef,
                                memoryPoolRef: ActorRef,
                                restApiSettings: RESTApiSettings)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("transactions") {
    //getUnconfirmedTransactionsR ~
      defaultTransferTransactionR
  }

  override val settings: RESTApiSettings = restApiSettings

 // private def getMempool: Future[MempoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m.get)

//  private def getUnconfirmedTransactions(limit: Int, offset: Int = 0): Future[Json] = getMempool.map {
//    _.unconfirmed.values.slice(offset, offset + limit)
//  }.map(_.map(_.asJson).asJson)

  def defaultTransferTransactionR: Route = path("send") {
    post(entity(as[Transaction]) {
      tx =>
        complete {
          memoryPoolRef ! LocallyGeneratedTransaction(tx)
          StatusCodes.OK
        }
    })
  }

//  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & paging) { (offset, limit) =>
//    getUnconfirmedTransactions(limit, offset).okJson()
//  }
}