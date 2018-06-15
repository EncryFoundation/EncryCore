package encry.settings

import java.io.File

import encry.utils.ByteStr

case class WalletSettings (seed: ByteStr,
                           password: String,
                           walletDir: File)
