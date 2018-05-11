package encry.consensus

import java.math.BigInteger

object DifficultySerializer {

  def toBytes(target: Difficulty): Array[Byte] = uint32ToByteArrayBE(encodeCompactBits(target))

  def parseBytes(array: Array[Byte]): Difficulty = Difficulty @@ decodeCompactBits(readUint32BE(array))

  private def decodeMPI(mpi: Array[Byte], hasLength: Boolean): BigInteger = {
    var buf: Array[Byte] = null // scalastyle:ignore
    if (hasLength) {
      val length: Int = readUint32BE(mpi).toInt
      buf = new Array[Byte](length)
      System.arraycopy(mpi, 4, buf, 0, length)
    } else {
      buf = mpi
    }
    if (buf.length != 0){
      val isNegative: Boolean = (buf(0) & 0x80) == 0x80
      if (isNegative) buf(0) = (buf(0) & 0x7f).toByte
      val result: BigInteger = new BigInteger(buf)
      if (isNegative) result.negate else result
      } else BigInteger.ZERO
  }

  def readUint32BE(bytes: Array[Byte]): NBits =
    NBits @@ (((bytes(0) & 0xffl) << 24) | ((bytes(1) & 0xffl) << 16) | ((bytes(2) & 0xffl) << 8) | (bytes(3) & 0xffl))

  def uint32ToByteArrayBE(value: NBits): Array[Byte] =
    Array(0xFF & (value >> 24), 0xFF & (value >> 16), 0xFF & (value >> 8), 0xFF & value).map(_.toByte)


  def decodeCompactBits(compact: NBits): BigInt = {
    val size: Int = (compact >> 24).toInt & 0xFF
    val bytes: Array[Byte] = new Array[Byte](4 + size)
    bytes(3) = size.toByte
    if (size >= 1) bytes(4) = ((compact >> 16) & 0xFF).toByte
    if (size >= 2) bytes(5) = ((compact >> 8) & 0xFF).toByte
    if (size >= 3) bytes(6) = (compact & 0xFF).toByte
    decodeMPI(bytes, hasLength = true)
  }

  def encodeCompactBits(requiredDifficulty: BigInt): NBits = {
    val value = requiredDifficulty.bigInteger
    var result: Long = 0L
    var size: Int = value.toByteArray.length
    if (size <= 3) {
      result = value.longValue << 8 * (3 - size)
    } else {
      result = value.shiftRight(8 * (size - 3)).longValue
    }
    // The 0x00800000 bit denotes the sign.
    // Thus, if it is already set, divide the mantissa by 256 and increase the exponent.
    if ((result & 0x00800000L) != 0) {
      result >>= 8
      size += 1
    }
    result |= size << 24
    val a: Int = if (value.signum == -1) 0x00800000 else 0
    result |= a
    NBits @@ result
  }
}
