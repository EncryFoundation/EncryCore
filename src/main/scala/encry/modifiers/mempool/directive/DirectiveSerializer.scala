package encry.modifiers.mempool.directive

import scorex.core.serialization.Serializer

import scala.util.{Failure, Try}

object DirectiveSerializer extends Serializer[Directive] {

  override def toBytes(obj: Directive): Array[Byte] = obj match {
    case td: TransferDirective =>
      TransferDirective.TypeId +: TransferDirectiveSerializer.toBytes(td)
    case m => throw new Error(s"Serialization for unknown modifier: ${m.json.noSpaces}")
  }

  override def parseBytes(bytes: Array[Byte]): Try[Directive] = Try(bytes.head).flatMap {
    case TransferDirective.`TypeId` => TransferDirectiveSerializer.parseBytes(bytes)
    case t => Failure(new Error(s"Got unknown typeId: $t"))
  }
}
