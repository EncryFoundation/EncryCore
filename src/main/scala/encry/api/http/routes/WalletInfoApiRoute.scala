package encry.api.http.routes

import akka.actor.{ ActorRef, ActorRefFactory }
import akka.http.scaladsl.server.Route
import akka.pattern._
import com.typesafe.scalalogging.StrictLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.api.http.DataHolderForApi.{ GetViewCreateKey, GetViewGetBalance, GetViewPrintAddress, GetViewPrintPubKeys }
import encry.settings.RESTApiSettings
import encry.view.actors.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import io.circe.syntax._
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.utils.Algos
import scala.concurrent.Future
import scala.util.Random

case class WalletInfoApiRoute(dataHolder: ActorRef, restApiSettings: RESTApiSettings)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute
    with FailFastCirceSupport
    with StrictLogging {

  override val route: Route = pathPrefix("wallet") {
    infoR ~ getUtxosR ~ printAddressR ~ createKeyR ~ printPubKeysR ~ getBalanceR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getWallet: Future[EncryWallet] =
    (dataHolder ?
      GetDataFromCurrentView[History, UtxoState, EncryWallet, EncryWallet](_.vault))
      .mapTo[EncryWallet]

  private def getAddresses: Future[String] = (dataHolder ? GetViewPrintAddress).mapTo[String]

  private def createKey: Future[PrivateKey25519] = (dataHolder ? GetViewCreateKey).mapTo[PrivateKey25519]

  private def pubKeys: Future[String] = (dataHolder ? GetViewPrintPubKeys).mapTo[String]

  private def getBalance: Future[String] = (dataHolder ? GetViewGetBalance).mapTo[String]

  def infoR: Route = (path("info") & get) {
    getWallet.map { w =>
      Map(
        "balances" -> w.getBalances.map(i => i._1 -> i._2.toString).toMap.asJson,
        "utxosQty" -> Random.shuffle(w.walletStorage.getAllBoxes(1000)).length.asJson
      ).asJson
    }.okJson()
  }

  def printAddressR: Route = (path("addr") & get) {
    getAddresses.map(_.asJson).okJson()
  }

  def createKeyR: Route = (path("createKey") & get) {
    createKey.map { x =>
      Algos.encode(x.privKeyBytes).asJson
    }.okJson()
  }

  def printPubKeysR: Route = (path("pubKeys") & get) {
    pubKeys.map(_.asJson).okJson()
  }

  def getBalanceR: Route = (path("balance") & get) {
    getBalance.map(_.asJson).okJson()
  }

  def getUtxosR: Route = (path("utxos") & get) {
    getWallet.map { w =>
      Random.shuffle(w.walletStorage.getAllBoxes(1000)).asJson
    }.okJson()
  }
}
