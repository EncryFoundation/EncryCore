package encry.crypto.encoding

import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success, Try}

object Base58Check {

  val Version: Byte = 1
  val ChecksumLength: Int = 4

  private def getChecksum(bytes: Array[Byte]): Array[Byte] = Blake2b256.hash(bytes).take(ChecksumLength)

  def encode(input: Array[Byte]): String = Base58.encode((Version +: input) ++ getChecksum(input))

  def decode(input: String): Try[Array[Byte]] = Base58.decode(input).flatMap { bytes =>
    val checksum: Array[Byte] = bytes.takeRight(ChecksumLength)
    val checksumActual: Array[Byte] = getChecksum(bytes.dropRight(ChecksumLength).tail)

    if (checksum.sameElements(checksumActual)) Success(bytes.dropRight(ChecksumLength).tail)
    else Failure(new Exception("Wrong checksum"))
  }
}
