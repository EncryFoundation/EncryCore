package encry.storage.codec

import io.iohk.iodb.ByteArrayWrapper

import scala.util.Try

object FixLenComplexValueCodec {

  def toComplexValue(vs: Seq[Array[Byte]]): ByteArrayWrapper =
    ByteArrayWrapper(vs.foldLeft(Array[Byte]())(_ ++ _))

  def parseComplexValue(bytes: Array[Byte], unitLen: Int): Try[Seq[Array[Byte]]] = Try {
    bytes.sliding(unitLen, unitLen).foldLeft(Seq[Array[Byte]]())(_ :+ _)
      .ensuring(bytes.length % unitLen == 0, "Value is inconsistent.")
  }
}
