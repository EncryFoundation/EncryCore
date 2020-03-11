package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.pattern._
import cats.syntax.eq._
import cats.kernel.Eq
import com.typesafe.scalalogging.StrictLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.EncryApp
import encry.api.http.DataHolderForApi._
import encry.modifiers.mempool.TransactionFactory
import encry.settings.RESTApiSettings
import encry.view.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.History
import encry.view.mempool.MemoryPool.NewTransaction
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import io.circe.syntax._
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.mempool.transaction.{PubKeyLockedContract, Transaction}
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBox, EncryProposition, MonetaryBox, TokenIssuingBox}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class WalletInfoApiRoute(dataHolder: ActorRef,
                              settings: RESTApiSettings,
                              intrinsicTokenId: String)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport with StrictLogging {

  implicit val eqOptBytes = new Eq[Option[Array[Byte]]] {
    override def eqv(x: Option[Array[Byte]], y: Option[Array[Byte]]) = {
      x.exists(l => y.exists(_.sameElements(l)))
    }
  }

  override val route: Route = pathPrefix("wallet") {
    infoR ~
      getUtxosR ~
      printAddressR ~
      printPubKeysR ~
      getBalanceR ~
    WebRoute.authRoute(
        createKeyR ~
        transferR ~
        transferContractR ~
        createTokenR ~
        dataTransactionR, settings
    )
  }

  private def getWallet: Future[EncryWallet] =
    (dataHolder ?
      GetDataFromCurrentView[History, UtxoState, EncryWallet, EncryWallet](_.vault))
      .mapTo[EncryWallet]

  def infoR: Route = (path("info") & get) {
    getWallet.map { w =>
      Map(
        "balances" -> w.getBalances.map{ i =>
          if (i._1._2 != intrinsicTokenId)
            s"TokenID(${i._1._2}) for contractHash ${i._1._1} : ${i._2}"
          else
            s"TokenID(${i._1._2}) for contractHash ${i._1._1} : ${BigDecimal(i._2) / 100000000}"
        }.asJson,
        "utxosQty" -> w.walletStorage.getAllBoxes().length.asJson
      ).asJson
    }.okJson()
  }

  def printAddressR: Route = (path("addr") & get) {
    (dataHolder ? GetViewPrintAddress)
      .mapTo[String].map(_.asJson).okJson()
  }

  def createTokenR: Route = (path("createToken") & get) {
    parameters('fee.as[Int], 'amount.as[Long]) { (fee, amount) =>
      (dataHolder ? GetDataFromCurrentView[History, UtxoState, EncryWallet, Option[Transaction]] {
        wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManagers.head.mandatoryAccount
            val boxes = wallet.vault.walletStorage
              .getAllBoxes().collectFirst { case ab: AssetBox => ab }.toList.take(1)
            TransactionFactory.assetIssuingTransactionScratch(
              secret,
              fee,
              System.currentTimeMillis(),
              boxes.map(_ -> None),
              PubKeyLockedContract(wallet.vault.accountManagers.head.mandatoryAccount.publicImage.pubKeyBytes).contract,
              amount)
          }.toOption
        }).flatMap {
        case Some(tx: Transaction) =>
          EncryApp.system.eventStream.publish(NewTransaction(tx))
          Future.unit
        case _ => Future.unit
      }
      complete("Token was created")
    }
    }


  def dataTransactionR: Route = (path("data") & get) {
    parameters('fee.as[Int], 'data) { (fee, data) =>
      (dataHolder ? GetDataFromCurrentView[History, UtxoState, EncryWallet, Option[Transaction]] {
        wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManagers.head.mandatoryAccount
            val boxes                   = wallet.vault.walletStorage
              .getAllBoxes().collectFirst { case ab: AssetBox => ab }.toList.take(1)
            TransactionFactory.dataTransactionScratch(secret,
              fee,
              System.currentTimeMillis(),
              boxes.map(_ -> None),
              PubKeyLockedContract(wallet.vault.accountManagers.head.mandatoryAccount.publicImage.pubKeyBytes).contract,
              data.getBytes)
          }.toOption
        }).flatMap {
        case Some(tx: Transaction) =>
          EncryApp.system.eventStream.publish(NewTransaction(tx))
          Future.unit
        case _ => Future.unit
      }
      complete(s"Tx with data has been sent!!'")
    }
  }

  def transferR: Route = (path("transfer") & get) {
    parameters('addr, 'fee.as[Int], 'amount.as[Long], 'token.?) { (addr, fee, amount, token) =>
      (dataHolder ? GetDataFromCurrentView[History, UtxoState, EncryWallet, Option[Transaction]] {
        wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManagers.head.mandatoryAccount
            val decodedTokenOpt = token.map(s => Algos.decode(s) match {
              case Success(value) => ADKey @@ value
              case Failure(e) => throw new RuntimeException(s"Failed to decode tokeId $s. Cause: $e")
            })
            val a: Seq[MonetaryBox] = wallet.vault.walletStorage
              .getAllBoxes()
              .collect {
                case ab: AssetBox if ab.tokenIdOpt.isEmpty || ab.tokenIdOpt === decodedTokenOpt => ab
                case tib: TokenIssuingBox if decodedTokenOpt.exists(_.sameElements(tib.tokenId)) => tib
              }

            val boxes: IndexedSeq[MonetaryBox] = wallet.vault.walletStorage
              .getAllBoxes()
              .collect {
                case ab: AssetBox if ab.tokenIdOpt.isEmpty || ab.tokenIdOpt === decodedTokenOpt => ab
                case tib: TokenIssuingBox if decodedTokenOpt.exists(_.sameElements(tib.tokenId)) => tib
              }.foldLeft(List.empty[MonetaryBox]) {
              case (seq, box) if decodedTokenOpt.isEmpty && seq.map(_.amount).sum < (amount + fee) => seq :+ box
              case (seq, _) if decodedTokenOpt.isEmpty => seq
              case (seq, box: AssetBox) if box.tokenIdOpt.isEmpty =>
                if (seq.collect { case ab: AssetBox if ab.tokenIdOpt.isEmpty => ab.amount }.sum < fee) seq :+ box else seq
              case (seq, box: AssetBox) =>
                val totalAmount =
                  seq.collect { case ab: AssetBox if ab.tokenIdOpt.nonEmpty => ab.amount }.sum +
                    seq.collect { case tib: TokenIssuingBox => tib.amount }.sum
                if (totalAmount < amount) seq :+ box else seq
              case (seq, box: TokenIssuingBox) =>
                val totalAmount =
                  seq.collect { case ab: AssetBox if ab.tokenIdOpt.nonEmpty => ab.amount }.sum +
                    seq.collect { case tib: TokenIssuingBox => tib.amount }.sum
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
    parameters('contract, 'fee.as[Int], 'amount.as[Long], 'token.?) {
      (contract, fee, amount, token) =>
      (dataHolder ? GetDataFromCurrentView[History, UtxoState, EncryWallet, Option[Transaction]] {
        wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManagers.head.mandatoryAccount
            val decodedTokenOpt         = token.map(s => Algos.decode(s) match {
              case Success(value) => ADKey @@ value
              case Failure(_) => throw new RuntimeException(s"Failed to decode tokeId $s")
            })
            val boxes: IndexedSeq[MonetaryBox] = wallet.vault.walletStorage
              .getAllBoxes()
              .collect {
                case ab: AssetBox if ab.tokenIdOpt.isEmpty || ab.tokenIdOpt === decodedTokenOpt => ab
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
    (dataHolder ? GetViewCreateKey)
      .mapTo[PrivateKey25519].map(key => Algos.encode(key.privKeyBytes).asJson).okJson()
  }

  def printPubKeysR: Route = (path("pubKeys") & get) {
    (dataHolder ? GetViewPrintPubKeys)
      .mapTo[List[String]].map(_.asJson).okJson()
  }

  def getBalanceR: Route = (path("balance") & get) {
    (dataHolder ? GetViewGetBalance)
      .mapTo[String].map(_.asJson).okJson()
  }

  def getUtxosR: Route = (path("utxos") & get) {
    getWallet.map(wallet => wallet.walletStorage.getAllBoxes().asJson).okJson()
  }
}