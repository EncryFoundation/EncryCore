package encry.utils

import encry.utils.LittleEndianBytes._
import org.bouncycastle.crypto.Digest

object Utils {

  def nonceToLeBytes(nonce: BigInt): Array[Byte] = {
    (for (i <- 0 to 7) yield leIntToByteArray((nonce >> 32 * i).intValue())).reduce(_ ++ _)
  }

  def hashNonce[T <: Digest](digest: T, nonce: BigInt): T = {
    val arr = nonceToLeBytes(nonce)
    digest.update(arr, 0, arr.length)
    digest
  }

  private val byteSize = 8

  def countLeadingZeroes(bytes: Array[Byte]): Byte = {
    (0 until byteSize * bytes.length).foldLeft(0.toByte) {
      case (res, i) if (bytes(i / byteSize) << i % byteSize & 0x80) == 0 => (res + 1).toByte
      case (res, _) => return res
    }
  }

  def validateSolution(solution: Array[Byte], target: Double): Boolean = {
    countLeadingZeroes(solution) >= target
  }
}
