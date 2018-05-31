package encry.cli.commands

import java.security.SecureRandom

import akka.pattern._
import akka.util.Timeout
import encry.cli.{Ast, Response}
import encry.settings.EncryAppSettings
import encry.utils.Mnemonic
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import encry.EncryApp._
import encry.view.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scala.concurrent.Future
import scala.util.Try

object InitKeyStorage extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    (nodeViewHolder ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[Response]] { view =>
        Try {
          val mnemonicCode = args.requireArgOrElse("seed", Ast.Str(Mnemonic.entropyToMnemonicCode(SecureRandom.getSeed(16)))).s
          view.vault.keyManager.initStorage(Mnemonic.mnemonicCodeToBytes(mnemonicCode))
          mnemonicCode
        }.toOption.map(code => Some(Response(s"Your mnemonic code is: $code"))).getOrElse(Some(Response("Operation failed. Couldn't init key storage.")))
      }).mapTo[Option[Response]]
  }
}
