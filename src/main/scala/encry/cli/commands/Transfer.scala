package encry.cli.commands

import akka.pattern._
import akka.util.Timeout
import encry.EncryApp._
import encry.Address
import encry.cli.{Ast, Response}
import encry.crypto.PrivateKey25519
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.settings.EncryAppSettings
import encry.utils.NetworkTime.Time
import encry.utils.NetworkTimeProvider
import encry.view.EncryNodeViewHolder.ReceivableMessages._
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet

import scala.concurrent.Future
import scala.util.Try

object Transfer extends Command {

  /**
    * Command "wallet transfer -addr=<addr[String]> -fee=<fee[Num]> -amount=<amount[Num]>"
    * Example "wallet transfer -addr='3jSD9fwHEHJwHq99ARqhnNhqGXeKnkJMyX4FZjHV6L3PjbCmjG' -fee=10000 -amount=2000"
    */
  // TODO: Notify `Vault` of spent boxes.
  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nodeViewHolder ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[Response]] { view =>
        Try {
          lazy val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)
          val secret: PrivateKey25519 = view.vault.keyManager.keys.head
          val recipient: Address = Address @@ args.requireArg[Ast.Str]("addr").s
          val fee: Long = args.requireArg[Ast.Num]("fee").i
          val amount: Long = args.requireArg[Ast.Num]("amount").i
          val timestamp: Time = timeProvider.time()
          val boxes: IndexedSeq[AssetBox] = view.vault.walletStorage.allBoxes.filter(_.isInstanceOf[AssetBox])
            .map(_.asInstanceOf[AssetBox]).foldLeft(Seq[AssetBox]()) { case (seq, box) =>
            if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
          }.toIndexedSeq

          val tx: EncryTransaction = TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, boxes, recipient, amount)

          nodeViewHolder ! LocallyGeneratedTransaction[EncryProposition, EncryTransaction](tx)

          tx
        }.toOption.map(tx => Some(Response(tx.toString))).getOrElse(Some(Response("Operation failed. Malformed data.")))
      }).mapTo[Option[Response]]
  }
}