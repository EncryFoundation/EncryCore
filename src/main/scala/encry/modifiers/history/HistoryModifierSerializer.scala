package encry.modifiers.history

//import BlockProto.AdProofsProtoMessage
//import HeaderProto.HeaderProtoMessage
import PayloadProto.PayloadProtoMessage
import encry.modifiers.EncryPersistentModifier
import org.encryfoundation.common.serialization.Serializer

import scala.util.{Failure, Success, Try}

object HistoryModifierSerializer extends Serializer[EncryPersistentModifier] {

  override def toBytes(obj: EncryPersistentModifier): Array[Byte] = obj match {
    case m: Header => Header.modifierTypeId +: HeaderSerializer.toBytes(m)
    case m: ADProofs => ADProofs.modifierTypeId +: ADProofSerializer.toBytes(m)
    case m: Payload => Payload.modifierTypeId +: PayloadSerializer.toBytes(m)
    case m => throw new Exception(s"Serialization for unknown modifier: $m")
  }

//  def toBytes1(obj: EncryPersistentModifier): Array[Byte] = obj match {
//    case m: Header => Header.modifierTypeId +: HeaderSerializer.toProto(m).toByteArray
//    case m: ADProofs => ADProofs.modifierTypeId +: ADProofSerializer.toProto(Some(m)).toByteArray
//    case m: Payload => Payload.modifierTypeId +: PayloadSerializer.toProto(m).toByteArray
//    case m => throw new Exception(s"Serialization for unknown modifier: $m")
//  }

//  def parseBytes1(bytes: Array[Byte]): Try[EncryPersistentModifier] = Try {
//    case _: Header if bytes.head == Header.`modifierTypeId` =>
//      Success(HeaderSerializer.fromProto(HeaderProtoMessage.parseFrom(bytes.tail)))
//    case _: ADProofs if bytes.head == ADProofs.`modifierTypeId` =>
//      Success(ADProofSerializer.fromProto(AdProofsProtoMessage.parseFrom(bytes.tail)))
//    case _: Payload if bytes.head == Payload.`modifierTypeId` =>
//      Success(PayloadSerializer.fromProto(PayloadProtoMessage.parseFrom(bytes.tail)))
//    case m => Failure(new Exception(s"Deserialization for unknown type byte: $m"))
//  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryPersistentModifier] =
    Try(bytes.head).flatMap {
      case Header.`modifierTypeId` => HeaderSerializer.parseBytes(bytes.tail)
      case ADProofs.`modifierTypeId` => ADProofSerializer.parseBytes(bytes.tail)
      case Payload.`modifierTypeId` => PayloadSerializer.parseBytes(bytes.tail)
      case m => Failure(new Exception(s"Deserialization for unknown type byte: $m"))
    }
}