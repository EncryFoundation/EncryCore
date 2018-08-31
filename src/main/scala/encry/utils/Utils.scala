package encry.utils

import com.google.common.primitives.Longs
import encry.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.modifiers.mempool.Transaction.Nonce
import encry.network.message.BasicMsgDataTypes.InvData
import encry.utils.LittleEndianBytes._
import org.bouncycastle.crypto.Digest
import org.encryfoundation.common.Algos

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
      case (res, _) => res
    }
  }

  def validateSolution(solution: Array[Byte], target: Double): Boolean = {
    countLeadingZeroes(solution) >= target
  }

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(Algos.hash(digest).take(8))

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)]): String = (ids.headOption, ids.lastOption) match {
    case (Some(f), Some(l)) if f._2 sameElements l._2 => s"[(${f._1},${Algos.encode(f._2)})]"
    case (Some(f), Some(l)) => s"[(${f._1},${Algos.encode(f._2)})..(${l._1},${Algos.encode(l._2)})]"
    case _ => "[]"
  }

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId]): String = {
    idsToString(ids.map(id => (modifierType, id)))
  }

  def idsToString(invData: InvData): String = idsToString(invData._1, invData._2)
}
