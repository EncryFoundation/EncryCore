package encry.cli.commands

import java.security.SecureRandom

import encry.cli.Ast
import encry.settings.EncryAppSettings
import encry.utils.Mnemonic

object InitKeyStorage extends ViewCommand {

  case class Request(mnemonicCode: String)

  override def executeRequest(args: Command.Args, settings: EncryAppSettings): Any =
    Request(args.requireArgOrElse("seed", Ast.Str(Mnemonic.entropyToMnemonicCode(SecureRandom.getSeed(16)))).s)

}
