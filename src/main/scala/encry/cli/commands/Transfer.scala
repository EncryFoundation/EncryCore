package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import cats.Applicative
import cats.implicits._
import encry.EncryApp
import encry.EncryApp._
import encry.cli.{Ast, Response}
import encry.modifiers.mempool.TransactionFactory
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.History
import encry.view.mempool.MemoryPool.NewTransaction
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.{AssetBox, MonetaryBox, TokenIssuingBox}
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
      GetDataFromCurrentView[History, UtxoState, EncryWallet, Option[Transaction]] { wallet =>
        Try {
          val secret: PrivateKey25519 = wallet.vault.accountManagers.head.mandatoryAccount
          val recipient: Address      = args.requireArg[Ast.Str]("addr").s
          val fee: Long               = args.requireArg[Ast.Num]("fee").i
          val amount: Long            = args.requireArg[Ast.Num]("amount").i
          val token                   = args.requireArgOrElse[Ast.Str]("token", Ast.Str("")).s
          val tokenOpt                = if (token.isEmpty) None else Some(token)
          val decodedTokenOpt         = tokenOpt.map(s => Algos.decode(s) match {
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
                                                       recipient,
                                                       amount,
                                                       decodedTokenOpt)
        }.toOption
      }).flatMap {
      case Some(tx: Transaction) =>
        EncryApp.system.eventStream.publish(NewTransaction(tx))
        Future.successful(Some(Response(tx.toString)))
      case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
    }
  }
}