package encry.crypto.encryption

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import scorex.crypto.hash.Blake2b256

object AES {

  val algorithm = "AES"

  private def cipher(mode: Int, secret: String): Cipher = {
    val encipher: Cipher = Cipher.getInstance(algorithm + "/ECB/PKCS5Padding")
    encipher.init(mode, new SecretKeySpec(Blake2b256.hash(secret).take(16), algorithm))
    encipher
  }

  def encrypt(data: Array[Byte], secret: String): Array[Byte] = cipher(Cipher.ENCRYPT_MODE, secret).doFinal(data)

  def decrypt(data: Array[Byte], secret: String): Array[Byte] = cipher(Cipher.DECRYPT_MODE, secret).doFinal(data)
}
