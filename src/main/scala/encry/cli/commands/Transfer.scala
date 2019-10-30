package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import cats.Applicative
import cats.implicits._
import encry.EncryApp._
import encry.api.http.DataHolderForApi.GetDataFromWallet
import encry.cli.Ast.Opt
import encry.cli.{Ast, Response}
import encry.modifiers.mempool.TransactionFactory
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.mempool.MemoryPool.NewTransaction
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Transfer extends Command {

  /**
   * Command "wallet transfer -addr=<addr[String]> -fee=<fee[Num]> -amount=<amount[Num]>"
   * Example "wallet transfer -addr='9fRWpnERVQKzR14qN5EGknx8xk11SU6LoZxcJAc53uAv3HRbL4K' -fee=10000 -amount=2000"
   */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ?
      GetDataFromWallet[Option[Transaction]] { wallet =>
        Try {
          val secret: PrivateKey25519 = wallet.accountManager.mandatoryAccount
          val recipient: Address      = args.requireArg[Ast.Str]("addr").s
          val fee: Long               = args.requireArg[Ast.Num]("fee").i
          val amount: Long            = args.requireArg[Ast.Num]("amount").i
          val tokenOpt                = args.requireArgOrElse[Ast.Opt[String]]("token", Opt(None)).opt
          val decodedTokenOpt         = tokenOpt.map(s => Algos.decode(s) match {
            case Success(value) => ADKey @@ value
            case Failure(_) => throw new RuntimeException(s"Failed to decode tokeId $s")
          })

          val boxes: IndexedSeq[AssetBox] = wallet.walletStorage
            .getAllBoxes()
            .collect {
              case ab: AssetBox if ab.tokenIdOpt.isEmpty ||
                Applicative[Option].map2(ab.tokenIdOpt, decodedTokenOpt)(_.sameElements(_)).getOrElse(true) => ab
            }
            .foldLeft(Seq[AssetBox]()) {
              case (seq, box) if decodedTokenOpt.isEmpty =>
                if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
              case (seq, box) if box.tokenIdOpt.isEmpty =>
                if (seq.filter(_.tokenIdOpt.isEmpty).map(_.amount).sum < fee) seq :+ box else seq
              case (seq, box) =>
                if (seq.filter(_.tokenIdOpt.nonEmpty).map(_.amount).sum < amount) seq :+ box else seq
            }
            .toIndexedSeq
          TransactionFactory.defaultPaymentTransaction(secret,
                                                       fee,
                                                       System.currentTimeMillis(),
                                                       boxes.map(_ -> None),
                                                       recipient,
                                                       amount,
                                                       decodedTokenOpt)
        }.toOption
      }).flatMap {
      case Some(tx: Transaction) =>
        memoryPool ! NewTransaction(tx)
        Future.successful(Some(Response(tx.toString)))
      case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
    }
  }
}