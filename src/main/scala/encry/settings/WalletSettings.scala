package encry.settings

import java.io.File

import scorex.core.utils.ByteStr

case class WalletSettings (seed: ByteStr,
                           password: String,
                           walletDir: File)
