package encry.api.http.routes

import akka.actor.{ ActorRef, ActorRefFactory }
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.settings.RESTApiSettings
import encry.mpg.MemoryPool._
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction

case class TransactionsApiRoute(dataHolder: ActorRef, memoryPoolRef: ActorRef, settings: RESTApiSettings)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute
    with FailFastCirceSupport {

  override val route: Route = pathPrefix("transactions") {
    WebRoute.authRoute(defaultTransferTransactionR, settings)
  }

  def defaultTransferTransactionR: Route = path("send") {
    post(entity(as[Transaction]) { tx =>
      complete {
        memoryPoolRef ! NewTransaction(tx)
        StatusCodes.OK
      }
    })
  }
}
