package encry.cli.commands

import java.security.SecureRandom

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings
import encry.utils.Mnemonic
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView

import scala.util.Try

object InitKeyStorage extends Command {

  override def execute(nodeViewHolderRef: ActorRef,
                       args: Command.Args, settings: EncryAppSettings): Option[Response] = Try {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    nodeViewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Unit] { view =>
        val mnemonicCode = args.args.get("seed").map(_.asInstanceOf[Ast.Str].s).getOrElse(Mnemonic.entropyToMnemonicCode(SecureRandom.getSeed(16)))
        println(s"Your mnemonic key is: $mnemonicCode")
        view.vault.keyManager.initStorage(Mnemonic.mnemonicCodeToBytes(mnemonicCode))
      }
  }.map(_ => Some(Response("OK"))).getOrElse(Some(Response("Operation failed")))
}
