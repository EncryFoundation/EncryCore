package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.cli.{Ast, Response}
import encry.modifiers.mempool.{Transaction, TransactionFactory}
import encry.modifiers.state.box.AssetBox
import encry.settings.EncryAppSettings
import encry.view.NodeViewHolder.ReceivableMessages._
import encry.view.history.EncryHistory
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.transaction.EncryAddress.Address
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

object Transfer extends Command {

  /**
    * Command "wallet transfer -addr=<addr[String]> -fee=<fee[Num]> -amount=<amount[Num]>"
    * Example "wallet transfer -addr='9fRWpnERVQKzR14qN5EGknx8xk11SU6LoZxcJAc53uAv3HRbL4K' -fee=10000 -amount=2000"
    */
  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       ncRef: ActorRef,
                       nvhRef: ActorRef,
                       minerRef: ActorRef,
                       nvshRef: ActorRef,
                       mempoolRef: ActorRef): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nvhRef ? GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, Option[Transaction]] { view =>
      Try {
        val secret: PrivateKey25519 = view.vault.accountManager.mandatoryAccount
        val recipient: Address = args.requireArg[Ast.Str]("addr").s
        val fee: Long = args.requireArg[Ast.Num]("fee").i
        val amount: Long = args.requireArg[Ast.Num]("amount").i
        val boxes: IndexedSeq[AssetBox] = view.vault.walletStorage.getAllBoxes().filter(_.isInstanceOf[AssetBox])
          .map(_.asInstanceOf[AssetBox]).foldLeft(Seq[AssetBox]()) { case (seq, box) =>
          if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
        }.toIndexedSeq
        TransactionFactory.defaultPaymentTransaction(
          secret,
          fee,
          System.currentTimeMillis(),
          boxes.map(_ -> None),
          recipient,
          amount)
      }.toOption
    }).flatMap {
      case Some(tx: Transaction) =>
        mempoolRef ! LocallyGeneratedTransaction(tx)
        Future.successful(Some(Response(tx.toString)))
      case _ => Future.successful(Some(Response("Operation failed. Malformed data.")))
    }
  }
}