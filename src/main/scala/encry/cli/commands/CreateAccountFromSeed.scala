package encry.cli.commands

import akka.util.Timeout
import akka.pattern._
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings
import encry.EncryApp._
import encry.view.NodeViewHolder.ReceivableMessages.{CreateAccountManagerFromSeed, NewAccountManager}

import scala.concurrent.Future

/**
  * Command "wallet fromSeed -seed=<seed[String]>"
  * Example: wallet fromSeed -seed='another accuse index island little scissors insect little absurd island keep little valid'"
  */

object CreateAccountFromSeed extends Command {
  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    val seed: String = args.requireArg[Ast.Str]("seed").s
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
    (nodeViewHolder ? CreateAccountManagerFromSeed(seed)).mapTo[NewAccountManager].map { s =>
      s.option match {
        case Some(wallet) => Some(Response(s"Created account manager #${wallet.accountManagers.map(_.number).max}"))
        case None => Some(Response(s"Failed to create new account manager"))
      }
    }
  }
}
