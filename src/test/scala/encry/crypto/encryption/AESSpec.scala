package encry.crypto.encryption

import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class AESSpec extends PropSpec with Matchers {

  property("Encrypt/Decrypt (Correct pass)") {

    val pass: String = "123456789"

    val data: Array[Byte] = "abcdefg".getBytes()

    val encrypted: Array[Byte] = AES.encrypt(data, pass)

    val decrypted: Array[Byte] = AES.decrypt(encrypted, pass)

    data sameElements decrypted shouldBe true
  }

  property("Encrypt/Decrypt (Wrong pass)") {

    val pass: String = "123456789"

    val data: Array[Byte] = "abcdefg".getBytes()

    val encrypted: Array[Byte] = AES.encrypt(data, pass)

    val decrypted: Try[Array[Byte]] = Try(AES.decrypt(encrypted, "wrong pass"))

    decrypted.isSuccess shouldBe false
  }
}
