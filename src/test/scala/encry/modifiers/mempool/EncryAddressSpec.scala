package encry.modifiers.mempool

import encry.Address
import encry.crypto.encoding.Base58Check
import encry.modifiers.mempool.regcontract.PubKeyLockedContract
import io.iohk.iodb.ByteArrayWrapper
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.{Curve25519, PublicKey}
import scorex.utils.Random

class EncryAddressSpec extends PropSpec with Matchers {

  val pubKey: PublicKey = Curve25519.createKeyPair(Random.randomBytes())._2
  val validP2PK: Address = Address @@ Base58Check.encode(Pay2PubKeyAddress.TypePrefix +: pubKey)
  val validP2CH: Address = Address @@ Base58Check.encode(Pay2ContractHashAddress.TypePrefix +: PubKeyLockedContract(pubKey).contract.hash)

  val invalidP2PK: Address = Address @@ Base58.encode(Random.randomBytes())
  val invalidP2CH: Address = Address @@ Base58.encode(Random.randomBytes())
  val invalidP2CHPrefix: Address = Address @@ Base58Check.encode(99.toByte +: Random.randomBytes())

  val p2pk: Pay2PubKeyAddress = Pay2PubKeyAddress(validP2PK)
  val p2ch: Pay2ContractHashAddress = Pay2ContractHashAddress(validP2CH)

  property("Addresses resolving") {

    EncryAddress.resolveAddress(validP2PK).isSuccess shouldBe true

    EncryAddress.resolveAddress(validP2CH).isSuccess shouldBe true

    EncryAddress.resolveAddress(invalidP2PK).isSuccess shouldBe false

    EncryAddress.resolveAddress(invalidP2CH).isSuccess shouldBe false

    EncryAddress.resolveAddress(invalidP2CHPrefix).isSuccess shouldBe false
  }

  property("p2pk to p2ch") {

    p2pk.p2ch shouldEqual p2ch
  }

  property("PubKey extraction") {

    ByteArrayWrapper(p2pk.pubKey) == ByteArrayWrapper(pubKey) shouldBe true
  }

  property("isValid()") {

    Pay2ContractHashAddress(validP2CH).isValid shouldBe true

    Pay2ContractHashAddress(invalidP2CH).isValid shouldBe false

    Pay2PubKeyAddress(validP2PK).isValid shouldBe true

    Pay2PubKeyAddress(invalidP2PK).isValid shouldBe false
  }
}
