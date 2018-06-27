package encry.modifiers.serialization

import scala.util.Try

trait Serializer[M] {

  def toBytes(obj: M): Array[Byte]

  def parseBytes(bytes: Array[Byte]): Try[M]
}
