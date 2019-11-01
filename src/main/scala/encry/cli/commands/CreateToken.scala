package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.EncryApp._
import encry.api.http.DataHolderForApi.{GetDataFromPresentView, GetViewCreateKey}
import encry.cli.{Ast, Response}
import encry.modifiers.mempool.TransactionFactory
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.history.History
import encry.view.mempool.MemoryPool.NewTransaction
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction.{PubKeyLockedContract, Transaction}
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBaseBox}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try


// wallet createToken -fee=100 -amount=50000
object CreateToken extends Command {

  override def execute(args: Command.Args,
                       settings: EncryAppSettings,
                       dataHolder: ActorRef,
                       nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (dataHolder ?
      GetDataFromPresentView[History, UtxoState, EncryWallet, Option[Transaction]] { wallet =>
        Try {
          val secret: PrivateKey25519 = wallet.vault.accountManager.mandatoryAccount
          val fee: Long               = args.requireArg[Ast.Num]("fee").i
          val amount: Long            = args.requireArg[Ast.Num]("amount").i
          val boxes: AssetBox         =     wallet.vault.walletStorage
            .getAllBoxes().collect { case ab: AssetBox => ab }.head
          println(boxes + " boxes")
          TransactionFactory.assetIssuingTransactionScratch(secret,
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
  }
}