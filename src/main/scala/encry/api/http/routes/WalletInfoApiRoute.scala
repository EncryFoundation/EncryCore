package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern._
import akka.util.Timeout
import com.typesafe.scalalogging.StrictLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.EncryApp.memoryPool
import encry.api.http.DataHolderForApi.{GetDataFromWallet, GetViewCreateKey, GetViewGetBalance, GetViewPrintAddress, GetViewPrintPubKeys}
import encry.cli.{Ast, Response}
import encry.modifiers.mempool.TransactionFactory
import encry.settings.RESTApiSettings
import encry.view.mempool.MemoryPool.NewTransaction
import encry.view.wallet.EncryWallet
import io.circe.syntax._
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.Algos

import scala.concurrent.Future
import scala.util.{Random, Try}

case class WalletInfoApiRoute(dataHolder: ActorRef, restApiSettings: RESTApiSettings)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute
    with FailFastCirceSupport
    with StrictLogging {

  override val route: Route = pathPrefix("wallet") {
    infoR ~ getUtxosR ~ printAddressR ~ createKeyR ~ printPubKeysR ~ getBalanceR ~ aaa
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getWallet: Future[EncryWallet] =
    (dataHolder ? GetDataFromWallet[EncryWallet](identity)).mapTo[EncryWallet]

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

  def aaa = (path("transfer") & get ) {
    parameters('addr, 'fee.as[Int], 'amount.as[Int]) { (addr, fee, amount) =>
      (dataHolder ?
        GetDataFromWallet[Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.accountManager.mandatoryAccount
//            val recipient: Address      = args.requireArg[Ast.Str]("addr").s
//            val fee: Long               = args.requireArg[Ast.Num]("fee").i
//            val amount: Long            = args.requireArg[Ast.Num]("amount").i
            val boxes: IndexedSeq[AssetBox] = wallet.walletStorage
              .getAllBoxes()
              .filter(_.isInstanceOf[AssetBox])
              .map(_.asInstanceOf[AssetBox])
              .foldLeft(Seq[AssetBox]()) {
                case (seq, box) =>
                  if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
              }
              .toIndexedSeq
            TransactionFactory.defaultPaymentTransaction(secret,
              fee,
              System.currentTimeMillis(),
              boxes.map(_ -> None),
              addr,
              amount)
          }.toOption
        }).flatMap {
        case Some(tx: Transaction) =>
          memoryPool ! NewTransaction(tx)
          Future.successful(Some(Response(tx.toString)))
        case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
      }
      complete(s"Tx with $addr, $fee and $amount has been sent!!'")
    }
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
