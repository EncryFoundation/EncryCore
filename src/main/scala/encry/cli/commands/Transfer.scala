package encry.cli.commands

import encry.EncryApp._
import encry.account.Address
import encry.cli.Ast
import encry.settings.EncryAppSettings
import encry.utils.NetworkTime.Time

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

  case class Request(recipient: Address, fee: Long, amount: Long, timestamp: Time)
}
