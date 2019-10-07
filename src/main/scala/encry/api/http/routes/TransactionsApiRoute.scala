package encry.api.http.routes

import akka.actor.{ ActorRef, ActorRefFactory }
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.settings.RESTApiSettings
import encry.view.mempool.MemoryPool.NewTransaction
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction

case class TransactionsApiRoute(dataHolder: ActorRef, memoryPoolRef: ActorRef, restApiSettings: RESTApiSettings)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute
    with FailFastCirceSupport {

  override val route: Route = pathPrefix("transactions") {
    defaultTransferTransactionR
  }

  override val settings: RESTApiSettings = restApiSettings

  def defaultTransferTransactionR: Route = path("send") {
    post(entity(as[Transaction]) { tx =>
      complete {
        memoryPoolRef ! NewTransaction(tx)
        StatusCodes.OK
      }
    })
  }
}
