package encry.crypto.equihash

import com.google.common.primitives.Ints
import encry.modifiers.serialization.{BytesSerializable, Serializer}
import encry.settings.Constants
import io.circe.Encoder

import scala.util.Try

case class EquihashSolution(ints: Seq[Int]) extends BytesSerializable {

  type M = EquihashSolution

  def indexedSeq: IndexedSeq[Int] = ints.toIndexedSeq

  def serializer: Serializer[EquihashSolution] = EquihashSolutionsSerializer
}

object EquihashSolution {

  val length: Int = Constants.Chain.HashLength
  def empty: EquihashSolution = EquihashSolution(Seq.fill(length)(0))

  /** This is for json representation of [[EquihashSolution]] instances */
  implicit val jsonEncoder: Encoder[EquihashSolution] =
    Encoder.encodeSeq[Int].contramap[EquihashSolution](_.ints)
}

object EquihashSolutionsSerializer extends Serializer[EquihashSolution] {

  override def toBytes(obj: EquihashSolution): Array[Byte] = {
    obj.ints.map(Ints.toByteArray).reduceLeft(_ ++ _)
  }

  override def parseBytes(bytes: Array[Byte]): Try[EquihashSolution] = Try {
    val seq = for {i <- bytes.indices by Ints.BYTES} yield {
      Ints.fromByteArray(bytes.slice(i, i + Ints.BYTES))
    }
    EquihashSolution(seq)
  }
}
