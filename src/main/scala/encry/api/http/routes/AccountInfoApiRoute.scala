package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.account.Address
import encry.view.EncryViewReadersHolder.{GetReaders, Readers}
import encry.view.state.UtxoStateReader
import io.circe.Json
import scorex.core.settings.RESTApiSettings

import scala.concurrent.Future

case class AccountInfoApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef,
                               restApiSettings: RESTApiSettings, digest: Boolean)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("account") {
    getKeyInfoByAddressR ~ getBoxesByAddressR ~ getPortfolioByAddressR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getState: Future[UtxoStateReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.s.get)

  private def getBoxesByAddress(address: Address): Future[Option[Json]] = ???

  private def getPortfolioByAddress(address: Address): Future[Option[Json]] = ???

  private def getKeyInfoByAddress(address: Address): Future[Option[Json]] = ???

  def getBoxesByAddressR: Route = (accountAddress & pathPrefix("boxes") & get) { addr =>
    getBoxesByAddress(addr).okJson()
  }

  def getPortfolioByAddressR: Route = (accountAddress & pathPrefix("portfolio") & get) { addr =>
    getPortfolioByAddress(addr).okJson()
  }

  def getKeyInfoByAddressR: Route = (accountAddress & pathPrefix("keys") & get) { addr =>
    getKeyInfoByAddress(addr).okJson()
  }
}
