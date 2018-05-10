package encry.cli.commands

import akka.actor.ActorRef
import akka.util.Timeout
import encry.cli.{Ast, Response}
import encry.settings.{Algos, EncryAppSettings}
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView

trait Command {

  def execute(nodeViewHolderRef: ActorRef, args: Command.Args, settings: EncryAppSettings): Option[Response]

  def testExecute(nodeViewHolderRef: ActorRef, args: Command.Args, settings: EncryAppSettings): Unit = {
    val message = GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[Response]] { view =>
      Option(Response(
        view.vault.getBalances.foldLeft("")((str, tokenInfo) => str.concat(s"TokenID(${Algos.encode(tokenInfo._1)}) : ${tokenInfo._2}\n"))
      ))
    }
    println(message)
    nodeViewHolderRef ! message
  }
}

object Command {

  case class Args(args: Map[String, Ast.Value]) {

    def requireArg[VT <: Ast.Value](n: String): VT = args.get(n).map {
      case vt: VT@unchecked => vt
      case _ => throw new Error("Wrong argument type.")
    }.getOrElse(throw new Error(s"Argument $n not found."))

    def requireArgOrElse[VT <: Ast.Value](key: String, default: => VT): VT = args.get(key).map {
      case vt: VT@unchecked => vt
      case _ => default
    }.getOrElse(default)
  }
}
