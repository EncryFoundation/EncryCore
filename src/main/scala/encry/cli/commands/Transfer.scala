package encry.cli.commands

import akka.pattern._
import akka.util.Timeout
import encry.EncryApp._
import encry.account.Address
import encry.cli.{Ast, Response}
import encry.crypto.PrivateKey25519
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import encry.view.EncryNodeViewHolder.ReceivableMessages._
import encry.utils.NetworkTime.Time
import encry.utils.NetworkTimeProvider

import scala.concurrent.Future
import scala.util.Try

object Transfer extends ViewCommand {

  /**
    * Command "wallet transfer -addr=<addr[String]> -fee=<fee[Num]> -amount=<amount[Num]>"
    * Example "wallet transfer -addr='3jSD9fwHEHJwHq99ARqhnNhqGXeKnkJMyX4FZjHV6L3PjbCmjG' -fee=10000 -amount=2000"
    */
  // TODO: Notify `Vault` of spent boxes.
  override def executeRequest(args: Command.Args, settings: EncryAppSettings): Request = {
    Request(
      Address @@ args.requireArg[Ast.Str]("addr").s,
      args.requireArg[Ast.Num]("fee").i,
      args.requireArg[Ast.Num]("amount").i,
      timeProvider.time()
    )
  }

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nodeViewHolder ? executeRequest(args, settings)).mapTo[Option[Response]]
  }

  case class Request(recipient: Address, fee: Long, amount: Long, timestamp: Time)
}
