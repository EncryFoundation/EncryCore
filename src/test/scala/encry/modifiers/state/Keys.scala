package encry.modifiers.state

import encry.utils.TestHelper
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}

trait Keys {

  val secrets: Seq[PrivateKey25519] = TestHelper.getOrGenerateKeys(TestHelper.Props.keysFilePath)

  val secret: PrivateKey25519 = secrets.head
  val publicKey: PublicKey25519 = secret.publicImage
}
