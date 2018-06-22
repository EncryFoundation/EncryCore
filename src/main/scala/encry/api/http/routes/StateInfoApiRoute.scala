package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.account.{Address, Portfolio}
import encry.local.scanner.EncryScanner.{GetIndexReader, IndexReader}
import encry.local.scanner.storage.EncryIndexReader
import encry.settings.Algos
import encry.utils.BalanceCalculator
import encry.view.EncryViewReadersHolder.{GetReaders, Readers}
import encry.view.state.{StateMode, UtxoStateReader}
import io.circe.Json
import io.circe.syntax._
import encry.settings.RESTApiSettings
import scorex.crypto.authds.ADKey

import scala.concurrent.Future

case class StateInfoApiRoute(readersHolder: ActorRef,
                             nodeViewActorRef: ActorRef,
                             scannerRef: ActorRef,
                             restApiSettings: RESTApiSettings,
                             stateMode: StateMode)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("state") {
    getBoxesByAddressR ~
      getPortfolioByAddressR ~
      getBoxByIdR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getState: Future[UtxoStateReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.s.get)

  private def getIndex: Future[EncryIndexReader] = (scannerRef ? GetIndexReader).mapTo[IndexReader].map(_.reader)

  private def getBoxIdsByAddress(address: Address): Future[Option[Seq[ADKey]]] = getIndex.map {
    _.boxIdsByAddress(address)
  }

  private def getBoxById(id: ADKey): Future[Option[Json]] = getState.map(_.boxById(id).map(_.asJson))

  private def getBoxesByAddress(address: Address): Future[Option[Json]] = getState.flatMap { s =>
    getBoxIdsByAddress(address).map(_.map(s.boxesByIds))
  }.map(_.map(_.map(_.asJson).asJson))

  private def getPortfolioByAddress(address: Address): Future[Option[Json]] = getState.flatMap { s =>
    getBoxIdsByAddress(address).map(_.map { ids =>
      val bxs = s.boxesByIds(ids)
      val balance = BalanceCalculator.balanceSheet(bxs).map { case (id, am) =>
        Algos.encode(id) -> am
      }
      Some(Portfolio(address, balance, bxs))
    })
  }.map(_.map(_.map(_.asJson).asJson))

  def getBoxByIdR: Route = (boxId & get) {getBoxById(_).okJson()}

  def getBoxesByAddressR: Route =
    (pathPrefix("boxes") & accountAddress & get) (getBoxesByAddress(_).okJson())

  def getPortfolioByAddressR: Route =
    (pathPrefix("portfolio") & accountAddress & get) (getPortfolioByAddress(_).okJson())
}
