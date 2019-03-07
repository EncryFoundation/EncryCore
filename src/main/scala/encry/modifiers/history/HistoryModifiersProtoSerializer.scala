package encry.modifiers.history

import BlockProto.BlockProtoMessage.AdProofsProtoMessage
import HeaderProto.HeaderProtoMessage
import PayloadProto.PayloadProtoMessage
import encry.modifiers.EncryPersistentModifier
import scala.util.{Failure, Try}

object HistoryModifiersProtoSerializer {

  def toProto(modifier: EncryPersistentModifier): Array[Byte] = modifier match {
    case m: Header => Header.modifierTypeId +: HeaderProtoSerializer.toProto(m).toByteArray
    case m: ADProofs => ADProofs.modifierTypeId +: ADProofsProtoSerializer.toProto(m).toByteArray
    case m: Payload => Payload.modifierTypeId +: PayloadProtoSerializer.toProto(m).toByteArray
    case m => throw new RuntimeException(s"Serialization for unknown modifier: $m")
  }

  def fromProto(bytes: Array[Byte]): Try[EncryPersistentModifier] = Try {
    bytes.head match {
      case Header.`modifierTypeId` =>
        HeaderProtoSerializer.fromProto(HeaderProtoMessage.parseFrom(bytes.tail))
      case ADProofs.`modifierTypeId` =>
        Try(ADProofsProtoSerializer.fromProto(AdProofsProtoMessage.parseFrom(bytes.tail)))
      case Payload.`modifierTypeId` =>
        PayloadProtoSerializer.fromProto(PayloadProtoMessage.parseFrom(bytes.tail))
      case m => Failure(new RuntimeException(s"Deserialization for unknown type byte: $m"))
    }
  }.flatten
}