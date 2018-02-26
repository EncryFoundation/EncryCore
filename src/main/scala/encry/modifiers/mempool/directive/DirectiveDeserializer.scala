package encry.modifiers.mempool.directive

import scala.util.{Failure, Try}

object DirectiveDeserializer {

  def parseBytes(bytes: Array[Byte], typeId: Byte): Try[Directive] = typeId match {
    case TransferDirective.`TypeId` => TransferDirectiveSerializer.parseBytes(bytes)
    case t => Failure(new Error(s"Got unknown typeId: $t"))
  }
}
