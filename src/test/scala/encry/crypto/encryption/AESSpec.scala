package encry.crypto.encryption

import org.scalatest.{Matchers, PropSpec}

class AESSpec extends PropSpec with Matchers {

  property("Encrypt/Decrypt") {

    val pass: String = "123456789"

    val data: Array[Byte] = "abcdefg".getBytes()

    val encrypted: Array[Byte] = AES.encrypt(data, pass)

    val decrypted: Array[Byte] = AES.decrypt(encrypted, pass)

    data sameElements decrypted shouldBe true
  }
}
