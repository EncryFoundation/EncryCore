package encry.api.http.routes

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.pattern._
import akka.util.Timeout
import io.circe.parser
import io.circe.generic.auto._
import com.typesafe.scalalogging.StrictLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.EncryApp.memoryPool
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

import scala.concurrent.duration._
import org.encryfoundation.common.modifiers.mempool.transaction.{PubKeyLockedContract, Transaction}
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition}
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import pdi.jwt.{JwtAlgorithm, JwtClaim, JwtSprayJson}
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
     infoR ~ loginRoute ~ getUtxosR ~ printAddressR ~ createKeyR ~ printPubKeysR ~ getBalanceR ~ transferR ~ createTokenR ~ dataTransactionR
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

  // JWT
  val superSecretPasswordDb = Map(
    "admin" -> "admin",
    "daniel" -> "Rockthejvm1!"
  )

  val algorithm = JwtAlgorithm.HS256
  val secretKey = "encry"

  def checkPassword(username: String, password: String): Boolean =
    superSecretPasswordDb.contains(username) && superSecretPasswordDb(username) == password

  def createToken(username: String, expirationPeriodInDays: Int): String = {
    val claims = JwtClaim(
      expiration = Some(System.currentTimeMillis() / 1000 + TimeUnit.DAYS.toSeconds(expirationPeriodInDays)),
      issuedAt = Some(System.currentTimeMillis() / 1000),
      issuer = Some("rockthejvm.com"),
      subject = Some(username)
    )

    JwtSprayJson.encode(claims, secretKey, algorithm) // JWT string
  }

  def isTokenExpired(token: String): Boolean = JwtSprayJson.decode(token, secretKey, Seq(algorithm)) match {
    case Success(claims) => claims.expiration.getOrElse(0L) < System.currentTimeMillis() / 1000
    case Failure(_) => true
  }

  def isTokenValid(token: String): Boolean = JwtSprayJson.isValid(token, secretKey, Seq(algorithm))

  def loginRoute =
    post {
      entity(as[LoginRequest]) {
        case LoginRequest(username, password) if checkPassword(username, password) =>
          val token = createToken(username, 1)
          respondWithHeader(RawHeader("Access-Token", token)) {
            complete(StatusCodes.OK)
          }
        case _ => complete(StatusCodes.Unauthorized)
      }
    }

  val authenticatedRoute =
    (path("secureEndpoint") & get) {
      optionalHeaderValueByName("Authorization") {
        case Some(token) =>
          if (isTokenValid(token)) {
            if (isTokenExpired(token)) {
              complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "Token expired."))
            } else {
              complete("User accessed authorized endpoint!")
            }
          } else {
            complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "Token is invalid, or has been tampered with."))
          }
        case _ => complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "No token provided!"))
      }
    }

  //  JWT

  def createTokenR: Route = (path("createToken") & get) {
    parameters('fee.as[Int], 'amount.as[Long]) { (fee, amount) =>
      (dataHolder ?
        GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManager.mandatoryAccount
            val boxes: AssetBox         = wallet.vault.walletStorage
              .getAllBoxes().collect { case ab: AssetBox => ab }.head
            println(boxes + " boxes")
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
        GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Transaction]] { wallet =>
          Try {
            val secret: PrivateKey25519 = wallet.vault.accountManager.mandatoryAccount
            val boxes: IndexedSeq[AssetBox] = wallet.vault.walletStorage
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
