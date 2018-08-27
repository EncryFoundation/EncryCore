package encry.utils

import org.encryfoundation.common.Algos
import scorex.crypto.encode.Base58
import scala.util.Try

case class ByteStr(arr: Array[Byte]) {
  override def equals(a: Any): Boolean = a match {
    case other: ByteStr => arr.sameElements(other.arr)
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(arr)

  lazy val encoded: String = Algos.encode(arr)

  override lazy val toString: String = encoded
}

object ByteStr {
  def decodeBase58(s: String): Try[ByteStr] = Base58.decode(s).map(ByteStr(_))
  val empty: ByteStr = ByteStr(Array.emptyByteArray)
}