package encry.modifiers.mempool.directive

import TransactionProto.DirectiveProtoMessage
import TransactionProto.DirectiveProtoMessage._
import org.encryfoundation.common.serialization.Serializer

import scala.util.{Failure, Try}

object DirectiveSerializer extends Serializer[Directive] {

  override def toBytes(obj: Directive): Array[Byte] = obj match {
    case td: TransferDirective => TransferDirective.TypeId +: TransferDirectiveSerializer.toBytes(td)
    case aid: AssetIssuingDirective => AssetIssuingDirective.TypeId +: AssetIssuingDirectiveSerializer.toBytes(aid)
    case sad: ScriptedAssetDirective => ScriptedAssetDirective.TypeId +: ScriptedAssetDirectiveSerializer.toBytes(sad)
    case dd: DataDirective => DataDirective.TypeId +: DataDirectiveSerializer.toBytes(dd)
    case m => throw new Exception(s"Serialization of unknown directive type: $m")
  }

  def toBytes1(obj: Directive): Array[Byte] = obj match {
    case td: TransferDirective => TransferDirective.TypeId +: td.toProto(td).toByteArray
    case aid: AssetIssuingDirective =>
      AssetIssuingDirective.TypeId +: aid.toProto(aid).toByteArray
    case sad: ScriptedAssetDirective =>
      ScriptedAssetDirective.TypeId +: sad.toProto(sad).toByteArray
    case dd: DataDirective =>
      DataDirective.TypeId +: dd.toProto(dd).toByteArray
    case m => throw new Exception(s"Serialization of unknown directive type: $m")
  }

//  def parseBytes1(bytes: Array[Byte]): Try[Nothing] = Try {
//    case a: TransferDirective if bytes.head == TransferDirective.`TypeId` =>
//      val k = DirectiveProtoMessage().withTransferDirective(TransferDirectiveProtoMessage.parseFrom(bytes.tail))
//      a.fromProto(k)
//    case a: AssetIssuingDirective if bytes.head == AssetIssuingDirective.`TypeId` =>
//      val k = DirectiveProtoMessage().withAssetIssuingDirective(AssetIssuingDirectiveProtoMessage.parseFrom(bytes.tail))
//      a.fromProto(k)
//    case a: ScriptedAssetDirective if bytes.head == ScriptedAssetDirective.`TypeId` =>
//      val k = DirectiveProtoMessage().withScriptedAssetDirective(ScriptedAssetDirectiveProtoMessage.parseFrom(bytes.tail))
//      a.fromProto(k)
//    case a: DataDirective if bytes.head == DataDirective.`TypeId` =>
//      val k = DirectiveProtoMessage().withDataDirective(DataDirectiveProtoMessage.parseFrom(bytes.tail))
//      a.fromProto(k)
//    case t => Failure(new Exception(s"Got unknown typeId: $t"))
//  }

  override def parseBytes(bytes: Array[Byte]): Try[Directive] = Try(bytes.head).flatMap {
    case TransferDirective.`TypeId` => TransferDirectiveSerializer.parseBytes(bytes.tail)
    case AssetIssuingDirective.`TypeId` => AssetIssuingDirectiveSerializer.parseBytes(bytes.tail)
    case ScriptedAssetDirective.`TypeId` => ScriptedAssetDirectiveSerializer.parseBytes(bytes.tail)
    case DataDirective.`TypeId` => DataDirectiveSerializer.parseBytes(bytes.tail)
    case t => Failure(new Exception(s"Got unknown typeId: $t"))
  }
}
