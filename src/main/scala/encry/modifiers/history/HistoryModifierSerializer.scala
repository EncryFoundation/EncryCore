package encry.modifiers.history

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.history.block.payload.{EncryBlockPayload, EncryBlockPayloadSerializer}
import scorex.core.serialization.Serializer

import scala.util.{Failure, Try}

object HistoryModifierSerializer extends Serializer[EncryPersistentModifier] {

  override def toBytes(obj: EncryPersistentModifier): Array[Byte] = obj match {
    case m: EncryBlockHeader =>
      EncryBlockHeader.modifierTypeId +: EncryBlockHeaderSerializer.toBytes(m)
    case m: ADProofs =>
      ADProofs.modifierTypeId +: ADProofSerializer.toBytes(m)
    case m: EncryBlockPayload =>
      EncryBlockPayload.modifierTypeId +: EncryBlockPayloadSerializer.toBytes(m)
    case m =>
      throw new Exception(s"Serialization for unknown modifier: $m")
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryPersistentModifier] =
    Try(bytes.head).flatMap {
      case EncryBlockHeader.`modifierTypeId` =>
        EncryBlockHeaderSerializer.parseBytes(bytes.tail)
      case ADProofs.`modifierTypeId` =>
        ADProofSerializer.parseBytes(bytes.tail)
      case EncryBlockPayload.`modifierTypeId` =>
        EncryBlockPayloadSerializer.parseBytes(bytes.tail)
      case m =>
        Failure(new Exception(s"Deserialization for unknown type byte: $m"))
    }
}