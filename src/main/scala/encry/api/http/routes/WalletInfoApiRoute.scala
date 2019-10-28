package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern._
import akka.util.Timeout
import com.typesafe.scalalogging.StrictLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.EncryApp.memoryPool
import encry.api.http.DataHolderForApi.{GetDataFromHistory, GetDataFromWallet, GetViewCreateKey, GetViewGetBalance, GetViewPrintAddress, GetViewPrintPubKeys}
import encry.cli.{Ast, Response}
import encry.modifiers.mempool.TransactionFactory
import encry.settings.{EncryAppSettings, RESTApiSettings}
import encry.storage.levelDb.versionalLevelDB.WalletVersionalLevelDB
import encry.utils.Mnemonic
import encry.view.history.History
import encry.view.mempool.MemoryPool.NewTransaction
import encry.view.wallet.EncryWallet
import io.circe.syntax._
import org.encryfoundation.common.crypto.PrivateKey25519
import scala.concurrent.duration._
import org.encryfoundation.common.modifiers.mempool.transaction.{PubKeyLockedContract, Transaction}
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition}
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.Algos
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success, Try}


case class WalletInfoApiRoute(dataHolder: ActorRef,
                              restApiSettings: RESTApiSettings,
                              settingsApp: EncryAppSettings)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute
  with FailFastCirceSupport
  with StrictLogging {

  override val route: Route = pathPrefix("wallet") {
    infoR ~ getUtxosR ~ printAddressR ~ createKeyR ~ printPubKeysR ~ getBalanceR ~ transferR ~ createTokenR ~ dataTransactionR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getWallet: Future[EncryWallet] =
    (dataHolder ? GetDataFromWallet[EncryWallet](identity)).mapTo[EncryWallet]

  private def getAddresses: Future[String] = (dataHolder ? GetViewPrintAddress)
    .mapTo[String]

  private def createKey: Future[PrivateKey25519] = (dataHolder ? GetViewCreateKey)
    .mapTo[PrivateKey25519]

  private def pubKeys: Future[List[String]] = (dataHolder ? GetViewPrintPubKeys)
    .mapTo[List[String]]

  private def getBalance: Future[String] = (dataHolder ? GetViewGetBalance)
    .mapTo[String]

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

  def createTokenR: Route = (path("createToken") & get) {
    parameters('fee.as[Int], 'amount.as[Long]) { (fee, amount) =>
      (dataHolder ?
        GetDataFromWallet[Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.accountManager.mandatoryAccount
            val boxes: AssetBox         = wallet.walletStorage
              .getAllBoxes().collect { case ab: AssetBox => ab }.head
            println(boxes + " boxes")
            TransactionFactory.assetIssuingTransactionScratch(
              secret,
              fee,
              System.currentTimeMillis(),
              IndexedSeq(boxes).map(_ -> None),
              PubKeyLockedContract(wallet.accountManager.mandatoryAccount.publicImage.pubKeyBytes).contract,
              amount)
          }.toOption
        }).flatMap {
        case Some(tx: Transaction) =>
          memoryPool ! NewTransaction(tx)
          Future.successful(Some(Response(tx.toString)))
        case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
      }
      complete("Token was created")
    }
    }

  def dataTransactionR: Route = (path("data") & get) {
    parameters('fee.as[Int], 'data) {(fee, data) =>
      (dataHolder ?
        GetDataFromWallet[Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.accountManager.mandatoryAccount
            val boxes: AssetBox         = wallet.walletStorage
              .getAllBoxes().collect { case ab: AssetBox => ab }.head
            TransactionFactory.dataTransactionScratch(secret,
              fee,
              System.currentTimeMillis(),
              IndexedSeq(boxes).map(_ -> None),
              PubKeyLockedContract(wallet.accountManager.mandatoryAccount.publicImage.pubKeyBytes).contract,
              data.getBytes)
          }.toOption
        }).flatMap {
        case Some(tx: Transaction) =>
          println(tx)
          memoryPool ! NewTransaction(tx)
          Future.successful(Some(Response(tx.toString)))
        case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
      }
      complete(s"Tx with data  has been sent!!'")
    }
  }

  def transferR: Route = (path("transfer") & get) {
    parameters('addr, 'fee.as[Int], 'amount.as[Long]) { (addr, fee, amount) =>
      (dataHolder ?
        GetDataFromWallet[Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.accountManager.mandatoryAccount
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
          println(tx)
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


  //  def connectPeer: Route = path("add") {
  //    post(entity(as[String]) { str =>
  //      println(str)
  //      complete {
  //        Try {
  //          val split = str.split(':')
  //          val a = (split(0), split(1).toInt)
  //          println(split)
  //          println(a)
  //          a
  //        } match {
  //          case Success((host, port)) =>
  //            dataHolder ! PeerAdd(new InetSocketAddress(host, port))
  //            StatusCodes.OK
  //          case Failure(_) =>
  //            StatusCodes.BadRequest
  //        }
  //      }
  //    })
  //  }
  def getUtxosR: Route = (path("utxos") & get) {
    getWallet.map { w =>
      Random.shuffle(w.walletStorage.getAllBoxes(1000)).asJson
    }.okJson()
  }
}
