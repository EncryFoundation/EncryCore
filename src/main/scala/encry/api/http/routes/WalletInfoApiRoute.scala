package encry.api.http.routes

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers
import akka.pattern._
import akka.util.Timeout
import cats.Applicative
import io.circe.parser
import io.circe.generic.auto._
import com.typesafe.scalalogging.StrictLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.EncryApp
import encry.api.http.DataHolderForApi.{GetDataFromHistory, GetDataFromPresentView, GetViewCreateKey, GetViewGetBalance, GetViewPrintAddress, GetViewPrintPubKeys}
import encry.cli.{Ast, Response}
import encry.modifiers.mempool.TransactionFactory
import encry.settings.{EncryAppSettings, RESTApiSettings}
import encry.storage.levelDb.versionalLevelDB.WalletVersionalLevelDB
import encry.utils.Mnemonic
import encry.view.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.History
import encry.view.mempool.MemoryPool.NewTransaction
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import io.circe.syntax._
import org.encryfoundation.common.crypto.PrivateKey25519
import PredefinedFromEntityUnmarshallers._
import scala.concurrent.duration._
import org.encryfoundation.common.modifiers.mempool.transaction.{PubKeyLockedContract, Transaction}
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition, MonetaryBox, TokenIssuingBox}
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success, Try}


case class WalletInfoApiRoute(dataHolder: ActorRef,
                              restApiSettings: RESTApiSettings,
                              intrinsicTokenId: String,
                              settingsApp: EncryAppSettings)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport with StrictLogging {

  override val route: Route = pathPrefix("wallet") {
     infoR ~ getUtxosR ~ printAddressR ~ createKeyR ~ printPubKeysR ~ getBalanceR ~ transferR ~ transferContractR ~ createTokenR ~ dataTransactionR
  }

  override val settings: RESTApiSettings = restApiSettings

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
        "balances" -> w.getBalances.map(i => i._1 -> i._2.toString).toMap.asJson,
        "utxosQty" -> Random.shuffle(w.walletStorage.getAllBoxes(1000)).length.asJson
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
            val secret: PrivateKey25519 = wallet.vault.accountManager.mandatoryAccount
            val boxes: AssetBox         = wallet.vault.walletStorage
              .getAllBoxes().collect { case ab: AssetBox => ab }.head
            TransactionFactory.assetIssuingTransactionScratch(
              secret,
              fee,
              System.currentTimeMillis(),
              IndexedSeq(boxes).map(_ -> None),
              PubKeyLockedContract(wallet.vault.accountManager.mandatoryAccount.publicImage.pubKeyBytes).contract,
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

//  def infoR: Route = (path("info") & get) {
  //    getWallet
  //      .map { w =>
  //        Map(
  //          "balances" -> w.getBalances.map{ i =>
  //            if (i._1._2 != intrinsicTokenId)
  //              s"TokenID(${i._1._2}) for contractHash ${i._1._1} : ${i._2}"
  //            else
  //              s"TokenID(${i._1._2}) for contractHash ${i._1._1} : ${BigDecimal(i._2) / 100000000}"
  //          }.asJson,
  //          "utxosQty" -> Random.shuffle(w.walletStorage.getAllBoxes(1000)).length.asJson
  //        ).asJson
  //      }
  //      .okJson()
  //  }

  def dataTransactionR: Route = (path("data") & get) {
    parameters('fee.as[Int], 'data) {(fee, data) =>
      (dataHolder ?
        GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManager.mandatoryAccount
            val boxes: AssetBox         = wallet.vault.walletStorage
              .getAllBoxes().collect { case ab: AssetBox => ab }.head
            TransactionFactory.dataTransactionScratch(secret,
              fee,
              System.currentTimeMillis(),
              IndexedSeq(boxes).map(_ -> None),
              PubKeyLockedContract(wallet.vault.accountManager.mandatoryAccount.publicImage.pubKeyBytes).contract,
              data.getBytes)
          }.toOption
        }).flatMap {
        case Some(tx: Transaction) =>
          EncryApp.system.eventStream.publish(NewTransaction(tx))
          //memoryPool !
          Future.successful(Some(Response(tx.toString)))
        case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
      }
      complete(s"Tx with data  has been sent!!'")
    }
  }
  import cats.implicits._
  def transferR: Route = (path("transfer") & get) {
    parameters('addr, 'fee.as[Int], 'amount.as[Long], 'token.?) { (addr, fee, amount, token) =>
      (dataHolder ?
        GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManager.mandatoryAccount
//           val token1 = if (token.contains("487291c237b68dd2ab213be6b5d1174666074a5afab772b600ea14e8285affab")) Some("") else token
            val decodedTokenOpt         = token.map(s => Algos.decode(s) match {
              case Success(value) => ADKey @@ value
              case Failure(_) => throw new RuntimeException(s"Failed to decode tokeId $s")
            })
            val boxes: IndexedSeq[MonetaryBox] = wallet.vault.walletStorage
              .getAllBoxes()
              .collect {
                case ab: AssetBox if ab.tokenIdOpt.isEmpty ||
                  Applicative[Option].map2(ab.tokenIdOpt, decodedTokenOpt)(_.sameElements(_)).getOrElse(false) => ab
                case tib: TokenIssuingBox if decodedTokenOpt.exists(_.sameElements(tib.tokenId)) => tib
              }.foldLeft(Seq[MonetaryBox]()) {
              case (seq, box) if decodedTokenOpt.isEmpty =>
                if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
              case (seq, box: AssetBox) if box.tokenIdOpt.isEmpty =>
                if (seq.collect{ case ab: AssetBox => ab }.filter(_.tokenIdOpt.isEmpty).map(_.amount).sum < fee) seq :+ box else seq
              case (seq, box: AssetBox) =>
                val totalAmount =
                  seq.collect{ case ab: AssetBox => ab }.filter(_.tokenIdOpt.nonEmpty).map(_.amount).sum +
                    seq.collect{ case tib: TokenIssuingBox => tib }.map(_.amount).sum
                if (totalAmount < amount) seq :+ box else seq
              case (seq, box: TokenIssuingBox) =>
                val totalAmount =
                  seq.collect{ case ab: AssetBox => ab }.filter(_.tokenIdOpt.nonEmpty).map(_.amount).sum +
                    seq.collect{ case tib: TokenIssuingBox => tib }.map(_.amount).sum
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

          Future.successful(Some(Response(tx.toString)))
        case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
      }
      complete(s"Tx with $addr, $fee and $amount has been sent!!'")
    }
  }


  def transferContractR: Route = (path("transferContract") & get) {
    parameters('contract, 'fee.as[Int], 'amount.as[Long], 'token.?) { (contract, fee, amount, token) =>
      (dataHolder ?
        GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManager.mandatoryAccount
            val decodedTokenOpt         = token.map(s => Algos.decode(s) match {
              case Success(value) => ADKey @@ value
              case Failure(_) => throw new RuntimeException(s"Failed to decode tokeId $s")
            })
            val boxes: IndexedSeq[MonetaryBox] = wallet.vault.walletStorage
              .getAllBoxes()
              .collect {
                case ab: AssetBox if ab.tokenIdOpt.isEmpty ||
                  Applicative[Option].map2(ab.tokenIdOpt, decodedTokenOpt)(_.sameElements(_)).getOrElse(false) => ab
                case tib: TokenIssuingBox if decodedTokenOpt.exists(_.sameElements(tib.tokenId)) => tib
              }.foldLeft(Seq[MonetaryBox]()) {
              case (seq, box) if decodedTokenOpt.isEmpty =>
                if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
              case (seq, box: AssetBox) if box.tokenIdOpt.isEmpty =>
                if (seq.collect{ case ab: AssetBox => ab }.filter(_.tokenIdOpt.isEmpty).map(_.amount).sum < fee) seq :+ box else seq
              case (seq, box: AssetBox) =>
                val totalAmount =
                  seq.collect{ case ab: AssetBox => ab }.filter(_.tokenIdOpt.nonEmpty).map(_.amount).sum +
                    seq.collect{ case tib: TokenIssuingBox => tib }.map(_.amount).sum
                if (totalAmount < amount) seq :+ box else seq
              case (seq, box: TokenIssuingBox) =>
                val totalAmount =
                  seq.collect{ case ab: AssetBox => ab }.filter(_.tokenIdOpt.nonEmpty).map(_.amount).sum +
                    seq.collect{ case tib: TokenIssuingBox => tib }.map(_.amount).sum
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
          print(tx)
          EncryApp.system.eventStream.publish(NewTransaction(tx))

          Future.successful(Some(Response(tx.toString)))
        case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
      }
      complete(s"Tx with $contract, $fee and $amount has been sent!!'")
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