package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.account.{Address, Balance, Portfolio}
import encry.local.scanner.EncryScanner.{GetIndexReader, IndexReader}
import encry.local.scanner.storage.EncryIndexReader
import encry.utils.BoxFilter
import encry.view.EncryViewReadersHolder.{GetReaders, Readers}
import encry.view.state.{StateMode, UtxoStateReader}
import io.circe.Json
import io.circe.syntax._
import scorex.core.settings.RESTApiSettings
import scorex.crypto.authds.ADKey

import scala.concurrent.Future

case class AccountInfoApiRoute(readersHolder: ActorRef,
                               nodeViewActorRef: ActorRef,
                               scannerRef: ActorRef,
                               restApiSettings: RESTApiSettings,
                               stateMode: StateMode)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("account") {
    getKeyInfoByAddressR ~ getBoxesByAddressR ~ getPortfolioByAddressR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getState: Future[UtxoStateReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.s.get)

  private def getIndex: Future[EncryIndexReader] = (scannerRef ? GetIndexReader).mapTo[IndexReader].map(_.reader)

  private def getBoxIdsByAddress(address: Address): Future[Option[Seq[ADKey]]] = getIndex.map {
    _.boxIdsByAddress(address)
  }

  private def getBoxesByAddress(address: Address): Future[Option[Json]] = getState.flatMap { s =>
    getBoxIdsByAddress(address).map(_.map(ids => s.boxesByIds(ids)))
  }.map(_.map(_.map(_.json).asJson))

  private def getPortfolioByAddress(address: Address): Future[Option[Json]] = getState.flatMap { s =>
    getBoxIdsByAddress(address).map(_.map { ids =>
      val bxs = s.boxesByIds(ids)
      val balance = Balance @@ BoxFilter.filterAmountCarryingBxs(bxs).map(_.amount).sum
      Some(Portfolio(address, balance, bxs))
    })
  }.map(_.map(_.map(_.json).asJson))

  private def getPubKeyInfoBxsByAddress(address: Address): Future[Option[Json]] = getState.flatMap { s =>
    getBoxIdsByAddress(address).map(_.map(ids => s.boxesByIds(ids)))
  }.map(_.map(bxs => BoxFilter.filterPubKeyInfoBxs(bxs).map(_.json).asJson))

  def getBoxesByAddressR: Route = (accountAddress & pathPrefix("boxes") & get) { addr =>
    getBoxesByAddress(addr).okJson()
  }

  def getPortfolioByAddressR: Route = (accountAddress & pathPrefix("portfolio") & get) { addr =>
    getPortfolioByAddress(addr).okJson()
  }

  def getKeyInfoByAddressR: Route = (accountAddress & pathPrefix("keys") & get) { addr =>
    getPubKeyInfoBxsByAddress(addr).okJson()
  }
}
