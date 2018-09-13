package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.modifiers.state.box.EncryBaseBox
import encry.settings.RESTApiSettings
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import io.circe.syntax._
import org.encryfoundation.common.Algos
import scala.concurrent.Future

case class WalletInfoApiRoute(nodeViewActorRef: ActorRef,
                              restApiSettings: RESTApiSettings)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("wallet") {
    infoR ~ getUtxosR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getWallet: Future[EncryWallet] = (nodeViewActorRef ?
    GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, EncryWallet](_.vault))
    .mapTo[EncryWallet]

  def infoR: Route = (path("info") & get) {
    getWallet
      .map { w =>
        Map(
          "balances" -> w.getBalances.map(i => Algos.encode(i._1) -> i._2.toString).toMap.asJson,
          "utxosQty" -> w.walletStorage.allBoxes.length.asJson
        ).asJson
      }
      .okJson()
  }

  def getUtxosR: Route = (path("utxos") & get) {
    getWallet
      .map { x =>
        val a: List[EncryBaseBox] = util.Random.shuffle(x.walletStorage.allBoxes.toList)
          a.take(2000).asJson
      }
      .okJson()
  }
}