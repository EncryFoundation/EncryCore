package encry.cli.commands

import akka.pattern._
import akka.util.Timeout
import encry.EncryApp._
import encry.cli.{Ast, Response}
import encry.modifiers.mempool.{Transaction, TransactionFactory}
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.settings.EncryAppSettings
import encry.view.EncryNodeViewHolder.ReceivableMessages._
import encry.view.mempool.Mempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.transaction.EncryAddress.Address
import scala.concurrent.Future
import scala.util.Try

object Transfer extends Command {

  /**
    * Command "wallet transfer -addr=<addr[String]> -fee=<fee[Num]> -amount=<amount[Num]>"
    * Example "wallet transfer -addr='3jSD9fwHEHJwHq99ARqhnNhqGXeKnkJMyX4FZjHV6L3PjbCmjG' -fee=10000 -amount=2000"
    */
  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nodeViewHolder ?
      GetDataFromCurrentView[UtxoState, EncryWallet, Mempool, Option[Transaction]] { view =>
        Try {
          val secret: PrivateKey25519 = view.vault.accountManager.mandatoryAccount
          val recipient: Address = args.requireArg[Ast.Str]("addr").s
          val fee: Long = args.requireArg[Ast.Num]("fee").i
          val amount: Long = args.requireArg[Ast.Num]("amount").i
          val boxes: IndexedSeq[AssetBox] = view.vault.walletStorage.allBoxes.filter(_.isInstanceOf[AssetBox])
            .map(_.asInstanceOf[AssetBox]).foldLeft(Seq[AssetBox]()) { case (seq, box) =>
            if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
          }.toIndexedSeq
          TransactionFactory.defaultPaymentTransactionScratch(secret, fee, 0L, boxes, recipient, amount)
        }.toOption
      }).flatMap {
        case Some(tx: Transaction) =>
          timeProvider
            .time()
            .map { time =>
              val txWithTimestamp = tx.copy(timestamp = time)
              nodeViewHolder ! LocallyGeneratedTransaction[EncryProposition, Transaction](txWithTimestamp)
              Some(Response(txWithTimestamp.toString))
            }
        case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
      }
  }
}