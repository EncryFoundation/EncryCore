package encry.crypto.equihash

import java.util
import com.google.common.primitives.Ints
import encry.settings.Constants
import io.circe.{Decoder, Encoder, HCursor}
import org.encryfoundation.common.serialization.{BytesSerializable, Serializer}
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
  implicit val jsonEncoder: Encoder[EquihashSolution] = Encoder.encodeSeq[Int].contramap[EquihashSolution](_.ints)

  implicit val jsonDecoder: Decoder[EquihashSolution] = (c: HCursor) =>
    for { equihashSolution <- c.as[List[Int]] } yield EquihashSolution(equihashSolution)
}

object EquihashSolutionsSerializer extends Serializer[EquihashSolution] {

  override def toBytes(obj: EquihashSolution): Array[Byte] = {
    obj.ints.map(Ints.toByteArray).reduceLeft(_ ++ _)
  }

  override def parseBytes(bytes: Array[Byte]): Try[EquihashSolution] = Try {
    val seq = for { i <- bytes.indices by Ints.BYTES } yield
      {Ints.fromByteArray(util.Arrays.copyOfRange(bytes, i, i + Ints.BYTES)) }
    EquihashSolution(seq)
  }
}