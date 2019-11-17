package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.pattern._
import cats.Apply
import com.typesafe.scalalogging.StrictLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.EncryApp
import encry.api.http.DataHolderForApi._
import encry.cli.Response
import encry.modifiers.mempool.TransactionFactory
import encry.settings.{EncryAppSettings, RESTApiSettings}
import encry.view.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.History
import encry.view.mempool.MemoryPool.NewTransaction
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import io.circe.syntax._
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.mempool.transaction.{PubKeyLockedContract, Transaction}
import org.encryfoundation.common.modifiers.state.box.{AssetBox, MonetaryBox, TokenIssuingBox}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import cats.instances.option._

case class WalletInfoApiRoute(dataHolder: ActorRef,
                              settings: RESTApiSettings,
                              intrinsicTokenId: String,
                              settingsApp: EncryAppSettings)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport with StrictLogging {

  override val route: Route = pathPrefix("wallet") {
    infoR ~
      getUtxosR ~
      printAddressR ~
      createKeyR ~
      printPubKeysR ~
      getBalanceR ~
      transferR ~
      transferContractR ~
      createTokenR ~
      dataTransactionR
  }

  private def getWallet: Future[EncryWallet] =
    (dataHolder ?
      GetDataFromCurrentView[History, UtxoState, EncryWallet, EncryWallet](_.vault))
      .mapTo[EncryWallet]

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
        "balances" -> w.getBalances.map{ i =>
          if (i._1._2 != intrinsicTokenId)
            s"TokenID(${i._1._2}) for contractHash ${i._1._1} : ${i._2}"
          else
            s"TokenID(${i._1._2}) for contractHash ${i._1._1} : ${BigDecimal(i._2) / 100000000}"
        }.asJson,
        "utxosQty" -> w.walletStorage.getAllBoxes(1000).length.asJson
      ).asJson
    }.okJson()
  }

  def printAddressR: Route = (path("addr") & get) {
    getAddresses.map(_.asJson).okJson()
  }

  def errorR: Route = path("restricted") {
    reject(ValidationRejection("Restricted!"))
  }

  case class LoginRequest(a:String, b:String)

  def createTokenR: Route = (path("createToken") & get) {
    parameters('fee.as[Int], 'amount.as[Long]) { (fee, amount) =>
      (dataHolder ?
        GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManagers.head.mandatoryAccount
            val boxes: AssetBox         = wallet.vault.walletStorage
              .getAllBoxes().collect { case ab: AssetBox => ab }.head
            TransactionFactory.assetIssuingTransactionScratch(
              secret,
              fee,
              System.currentTimeMillis(),
              IndexedSeq(boxes).map(_ -> None),
              PubKeyLockedContract(wallet.vault.accountManagers.head.mandatoryAccount.publicImage.pubKeyBytes).contract,
              amount)
          }.toOption
        }).flatMap {
        case Some(tx: Transaction) =>
          EncryApp.system.eventStream.publish(NewTransaction(tx))
          Future.successful(Some(Response(tx.toString)))
        case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
      }
      complete("Token was created")
    }
    }


  def dataTransactionR: Route = (path("data") & get) {
    parameters('fee.as[Int], 'data) {(fee, data) =>
      (dataHolder ?
        GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManagers.head.mandatoryAccount
            val boxes: AssetBox         = wallet.vault.walletStorage
              .getAllBoxes().collect { case ab: AssetBox => ab }.head
            TransactionFactory.dataTransactionScratch(secret,
              fee,
              System.currentTimeMillis(),
              IndexedSeq(boxes).map(_ -> None),
              PubKeyLockedContract(wallet.vault.accountManagers.head.mandatoryAccount.publicImage.pubKeyBytes).contract,
              data.getBytes)
          }.toOption
        }).flatMap {
        case Some(tx: Transaction) =>
          EncryApp.system.eventStream.publish(NewTransaction(tx))
          Future.successful(Some(Response(tx.toString)))
        case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
      }
      complete(s"Tx with data  has been sent!!'")
    }
  }

  def transferR: Route = (path("transfer") & get) {
    parameters('addr, 'fee.as[Int], 'amount.as[Long], 'token.?) { (addr, fee, amount, token) =>
      (dataHolder ?
        GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManagers.head.mandatoryAccount
            val decodedTokenOpt         = token.map(s => Algos.decode(s) match {
              case Success(value) => ADKey @@ value
              case Failure(_) => throw new RuntimeException(s"Failed to decode tokeId $s")
            })
            val boxes: IndexedSeq[MonetaryBox] = wallet.vault.walletStorage
              .getAllBoxes()
              .collect {
                case ab: AssetBox if ab.tokenIdOpt.isEmpty ||
                  Apply[Option].map2(ab.tokenIdOpt, decodedTokenOpt)(_.sameElements(_)).getOrElse(false) => ab
                case tib: TokenIssuingBox if decodedTokenOpt.exists(_.sameElements(tib.tokenId)) => tib
              }.foldLeft(List.empty[MonetaryBox]) {
              case (seq, box) if decodedTokenOpt.isEmpty =>
                if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
              case (seq, box: AssetBox) if box.tokenIdOpt.isEmpty =>
                if (seq.collect{ case ab: AssetBox if ab.tokenIdOpt.isEmpty => ab.amount }.sum < fee) seq :+ box else seq
              case (seq, box: AssetBox) =>
                val totalAmount =
                  seq.collect { case ab: AssetBox if ab.tokenIdOpt.nonEmpty => ab.amount }.sum +
                    seq.collect{ case tib: TokenIssuingBox => tib.amount }.sum
                if (totalAmount < amount) seq :+ box else seq
              case (seq, box: TokenIssuingBox) =>
                val totalAmount =
                  seq.collect{ case ab: AssetBox if ab.tokenIdOpt.nonEmpty => ab.amount}.sum +
                    seq.collect{ case tib: TokenIssuingBox => tib.amount }.sum
                if (totalAmount < amount) seq :+ box else seq
            }
              .toIndexedSeq
            TransactionFactory.defaultPaymentTransaction(secret,
              fee,
              System.currentTimeMillis(),
              boxes.map(_ -> None),
              addr,
              amount,
              decodedTokenOpt)
          }.toOption
        }).flatMap {
        case Some(tx: Transaction) =>
          EncryApp.system.eventStream.publish(NewTransaction(tx))
          Future.unit
        case _ => Future.unit
      }
      complete(s"Tx with $addr, $fee and $amount has been sent!!'")
    }
  }


  def transferContractR: Route = (path("transferContract") & get) {
    parameters('contract, 'fee.as[Int], 'amount.as[Long], 'token.?) { (contract, fee, amount, token) =>
      (dataHolder ?
        GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManagers.head.mandatoryAccount
            val decodedTokenOpt         = token.map(s => Algos.decode(s) match {
              case Success(value) => ADKey @@ value
              case Failure(_) => throw new RuntimeException(s"Failed to decode tokeId $s")
            })
            val boxes: IndexedSeq[MonetaryBox] = wallet.vault.walletStorage
              .getAllBoxes()
              .collect {
                case ab: AssetBox if ab.tokenIdOpt.isEmpty ||
                  Apply[Option].map2(ab.tokenIdOpt, decodedTokenOpt)(_.sameElements(_)).getOrElse(false) => ab
                case tib: TokenIssuingBox if decodedTokenOpt.exists(_.sameElements(tib.tokenId)) => tib
              }.foldLeft(List.empty[MonetaryBox]) {
              case (seq, box) if decodedTokenOpt.isEmpty =>
                if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
              case (seq, box: AssetBox) if box.tokenIdOpt.isEmpty =>
                if (seq.collect{ case ab: AssetBox if ab.tokenIdOpt.isEmpty => ab.amount }.sum < fee) seq :+ box else seq
              case (seq, box: AssetBox) =>
                val totalAmount =
                  seq.collect { case ab: AssetBox if ab.tokenIdOpt.nonEmpty => ab.amount }.sum +
                    seq.collect{ case tib: TokenIssuingBox => tib.amount }.sum
                if (totalAmount < amount) seq :+ box else seq
              case (seq, box: TokenIssuingBox) =>
                val totalAmount =
                  seq.collect{ case ab: AssetBox if ab.tokenIdOpt.nonEmpty => ab.amount}.sum +
                    seq.collect{ case tib: TokenIssuingBox => tib.amount }.sum
                if (totalAmount < amount) seq :+ box else seq
            }
              .toIndexedSeq
            TransactionFactory.defaultContractTransaction(secret,
              fee,
              System.currentTimeMillis(),
              boxes.map(_ -> None),
              contract,
              amount,
              decodedTokenOpt)
          }.toOption
        }).flatMap {
        case Some(tx: Transaction) =>
          EncryApp.system.eventStream.publish(NewTransaction(tx))
          Future.unit
        case _ => Future.unit
      }
      complete(s"Tx with $contract, $fee and $amount has been sent!!'")
    }
  }

  def createKeyR: Route = (path("createKey") & get) {
    createKey.map(key => Algos.encode(key.privKeyBytes).asJson).okJson()
  }

  def printPubKeysR: Route = (path("pubKeys") & get) {
    pubKeys.map(_.asJson).okJson()
  }

  def getBalanceR: Route = (path("balance") & get) {
    getBalance.map(_.asJson).okJson()
  }

  def getUtxosR: Route = (path("utxos") & get) {
    getWallet.map(wallet => wallet.walletStorage.getAllBoxes(1000).asJson).okJson()
  }
}