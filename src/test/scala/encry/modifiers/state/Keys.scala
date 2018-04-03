package encry.modifiers.state

import encry.account.Account
import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.local.TestHelper

trait Keys {

  val secret: PrivateKey25519 = TestHelper.getOrGenerateKeys(TestHelper.Props.keysFilePath).head
  val publicKey: PublicKey25519 = secret.publicImage
  val account: Account = Account(publicKey.pubKeyBytes)
}
