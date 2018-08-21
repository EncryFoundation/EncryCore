package scorex.crypto.hash

import scorex.utils.ByteArray
import scala.util.Try

trait CryptographicHash[D <: Digest] {

  type Message = Array[Byte]

  val DigestSize: Int // in bytes

  def apply(input: Message): D = hash(input)

  def apply(input: String): D = hash(input.getBytes)

  def hash(input: Message): D

  def hash(input: String): D = hash(input.getBytes)

  def prefixedHash(prefix: Byte, inputs: Array[Byte]*): D = hash(prefix +: ByteArray.concat(inputs))

  def hash(inputs: Array[Byte]*): D = hash(ByteArray.concat(inputs))

  def byteArrayToDigest(bytes: Array[Byte]): Try[D]
}

