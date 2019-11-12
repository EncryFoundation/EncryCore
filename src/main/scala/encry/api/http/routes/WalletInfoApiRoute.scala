package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern._
import com.typesafe.scalalogging.StrictLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.EncryApp.nodeViewHolder
import encry.settings.RESTApiSettings
import encry.view.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import io.circe.syntax._

import scala.concurrent.Future
import scala.util.Random

case class WalletInfoApiRoute(dataHolder: ActorRef,
                              restApiSettings: RESTApiSettings,
                              intrinsicTokenId: String)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport with StrictLogging {

  override val route: Route = pathPrefix("wallet") { infoR ~ getUtxosR }

  override val settings: RESTApiSettings = restApiSettings

  private def getWallet: Future[EncryWallet] = (nodeViewHolder ?
    GetDataFromCurrentView[History, UtxoState, EncryWallet, EncryWallet](_.vault))
    .mapTo[EncryWallet]

  def infoR: Route = (path("info") & get) {
    getWallet
      .map { w =>
        Map(
          "balances" -> w.getBalances.map{ i =>
            if (i._1._2 != intrinsicTokenId)
              s"TokenID(${i._1._2}) for contractHash ${i._1._1} : ${i._2}"
            else
              s"TokenID(${i._1._2}) for contractHash ${i._1._1} : ${BigDecimal(i._2) / 100000000}"
          }.asJson,
          "utxosQty" -> Random.shuffle(w.walletStorage.getAllBoxes(1000)).length.asJson
        ).asJson
      }
      .okJson()
  }

  def getUtxosR: Route = (path("utxos") & get) {
    getWallet
      .map { w => Random.shuffle(w.walletStorage.getAllBoxes(1000)).asJson }
      .okJson()
  }
}