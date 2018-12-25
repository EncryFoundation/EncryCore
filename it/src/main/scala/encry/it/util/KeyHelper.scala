package encry.it.util

import encry.utils.Mnemonic
import org.encryfoundation.common.crypto.PrivateKey25519
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

object KeyHelper {

  def createPrivKey(seed: Option[String]): PrivateKey25519 = {
    val (privateKey: PrivateKey, publicKey: PublicKey) = Curve25519.createKeyPair(
      Blake2b256.hash(
        seed.map { Mnemonic.seedFromMnemonic(_) }
          .getOrElse {
            val phrase: String = Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16))
            Mnemonic.seedFromMnemonic(phrase)
          })
    )
    PrivateKey25519(privateKey, publicKey)
  }
}