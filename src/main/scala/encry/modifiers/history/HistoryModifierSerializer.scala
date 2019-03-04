package encry.modifiers.history

import BlockProto.BlockProtoMessage.AdProofsProtoMessage
import HeaderProto.HeaderProtoMessage
import PayloadProto.PayloadProtoMessage
import encry.modifiers.EncryPersistentModifier
import org.encryfoundation.common.serialization.Serializer
import scala.util.{Failure, Success, Try}

//object HistoryModifierSerializer extends Serializer[EncryPersistentModifier] {
//
//  override def toBytes(obj: EncryPersistentModifier): Array[Byte] = obj match {
//    case m: Header => Header.modifierTypeId +: HeaderSerializer.toBytes(m)
//    case m: ADProofs => ADProofs.modifierTypeId +: ADProofSerializer.toBytes(m)
//    case m: Payload => Payload.modifierTypeId +: PayloadSerializer.toBytes(m)
//    case m => throw new Exception(s"Serialization for unknown modifier: $m")
//  }
//
//  override def parseBytes(bytes: Array[Byte]): Try[EncryPersistentModifier] =
//    Try(bytes.head).flatMap {
//      case Header.`modifierTypeId` => HeaderSerializer.parseBytes(bytes.tail)
//      case ADProofs.`modifierTypeId` => ADProofSerializer.parseBytes(bytes.tail)
//      case Payload.`modifierTypeId` => PayloadSerializer.parseBytes(bytes.tail)
//      case m => Failure(new Exception(s"Deserialization for unknown type byte: $m"))
//    }
//}

object HistoryModifiersProtoSerializer {

  def toProto(modifier: EncryPersistentModifier): Array[Byte] = modifier match {
    case m: Header => Header.modifierTypeId +: HeaderProtoSerializer.toProto(m).toByteArray
    case m: ADProofs => ADProofs.modifierTypeId +: ADProofsProtoSerializer.toProto(m).toByteArray
    case m: Payload => Payload.modifierTypeId +: PayloadProtoSerializer.toProto(m).toByteArray
    case m => throw new RuntimeException(s"Serialization for unknown modifier: $m")
  }

  def fromProto(bytes: Array[Byte]): Try[EncryPersistentModifier] = Try(bytes.head).flatMap {
    case _ if bytes.head == Header.`modifierTypeId` =>
      HeaderProtoSerializer.fromProto(HeaderProtoMessage.parseFrom(bytes.tail))
    case _ if bytes.head == ADProofs.`modifierTypeId` =>
      Try(ADProofsProtoSerializer.fromProto(AdProofsProtoMessage.parseFrom(bytes.tail)))
    case _ if bytes.head == Payload.`modifierTypeId` =>
      PayloadProtoSerializer.fromProto(PayloadProtoMessage.parseFrom(bytes.tail))
    case m => Failure(new RuntimeException(s"Deserialization for unknown type byte: $m"))
  }
}