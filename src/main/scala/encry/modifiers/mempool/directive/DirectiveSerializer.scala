package encry.modifiers.mempool.directive

import TransactionProto.TransactionProtoMessage.DirectiveProtoMessage
import TransactionProto.TransactionProtoMessage.DirectiveProtoMessage.DirectiveProto
import org.encryfoundation.common.serialization.Serializer

import scala.util.{Failure, Try}

trait ProtoDirectiveSerializer[T] {

  def toProto(message: T): DirectiveProtoMessage

  def fromProto(message: DirectiveProtoMessage): Option[T]
}

object DirectiveProtoSerializer {
  def fromProto(message: DirectiveProtoMessage): Option[Directive] = message.directiveProto match {
    case DirectiveProto.AssetIssuingDirectiveProto(_) => AssetIssuingDirectiveProtoSerializer.fromProto(message)
    case DirectiveProto.DataDirectiveProto(_) => DataDirectiveProtoSerializer.fromProto(message)
    case DirectiveProto.TransferDirectiveProto(_) => TransferDirectiveProtoSerializer.fromProto(message)
    case DirectiveProto.ScriptedAssetDirectiveProto(_) => ScriptedAssetDirectiveProtoSerializer.fromProto(message)
    case DirectiveProto.Empty => None
  }
}

object DirectiveSerializer extends Serializer[Directive] {

  override def toBytes(obj: Directive): Array[Byte] = obj match {
    case td: TransferDirective => TransferDirective.TypeId +: TransferDirectiveSerializer.toBytes(td)
    case aid: AssetIssuingDirective => AssetIssuingDirective.TypeId +: AssetIssuingDirectiveSerializer.toBytes(aid)
    case sad: ScriptedAssetDirective => ScriptedAssetDirective.TypeId +: ScriptedAssetDirectiveSerializer.toBytes(sad)
    case dd: DataDirective => DataDirective.TypeId +: DataDirectiveSerializer.toBytes(dd)
    case m => throw new Exception(s"Serialization of unknown directive type: $m")
  }

  override def parseBytes(bytes: Array[Byte]): Try[Directive] = Try(bytes.head).flatMap {
    case TransferDirective.`TypeId` => TransferDirectiveSerializer.parseBytes(bytes.tail)
    case AssetIssuingDirective.`TypeId` => AssetIssuingDirectiveSerializer.parseBytes(bytes.tail)
    case ScriptedAssetDirective.`TypeId` => ScriptedAssetDirectiveSerializer.parseBytes(bytes.tail)
    case DataDirective.`TypeId` => DataDirectiveSerializer.parseBytes(bytes.tail)
    case t => Failure(new Exception(s"Got unknown typeId: $t"))
  }
}
