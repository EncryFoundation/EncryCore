//package encry.cli.commands
//
//import akka.util.Timeout
//import akka.pattern._
//import encry.cli.{Ast, Response}
//import encry.settings.EncryAppSettings
//import encry.EncryApp._
//import encry.view.NodeViewHolder.ReceivableMessages.CreateAccountManagerFromSeed
//import encry.view.wallet.EncryWallet
//
//import scala.concurrent.Future
//
///**
//  * Command:  wallet fromSeed -seed=<seed[String]>
//  * Example: wallet fromSeed -seed='another accuse index island little scissors insect little absurd island keep valid'
//  */
//
//object CreateAccountFromSeed extends Command {
//  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
//    val seed: String = args.requireArg[Ast.Str]("seed").s
//    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
//    (nodeViewHolder ? CreateAccountManagerFromSeed(seed)).mapTo[Either[String, EncryWallet]].map {
//      case Right(wallet) => Some(Response(s"Created account manager #${wallet.accountManagers.map(_.number).max}"))
//      case Left(reasons) => Some(Response(s"Failed to create new account manager:\n$reasons"))
//    }
//  }
//}
