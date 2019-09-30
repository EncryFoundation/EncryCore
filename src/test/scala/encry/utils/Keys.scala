package encry.utils

import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}

trait Keys {
  val mnemonicKey: String = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
  val privKey: PrivateKey25519 = Utils.createPrivKey(Some(mnemonicKey))
  val publicKey: PublicKey25519 = privKey.publicImage
}
