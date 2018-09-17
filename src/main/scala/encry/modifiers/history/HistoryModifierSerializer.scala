package encry.modifiers.history

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.payload.{Payload, EncryBlockPayloadSerializer}
import org.encryfoundation.common.serialization.Serializer
import scala.util.{Failure, Try}

object HistoryModifierSerializer extends Serializer[EncryPersistentModifier] {

  override def toBytes(obj: EncryPersistentModifier): Array[Byte] = obj match {
    case m: Header =>
      Header.modifierTypeId +: EncryBlockHeaderSerializer.toBytes(m)
    case m: ADProofs =>
      ADProofs.modifierTypeId +: ADProofSerializer.toBytes(m)
    case m: Payload =>
      Payload.modifierTypeId +: EncryBlockPayloadSerializer.toBytes(m)
    case m =>
      throw new Exception(s"Serialization for unknown modifier: $m")
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryPersistentModifier] =
    Try(bytes.head).flatMap {
      case Header.`modifierTypeId` =>
        EncryBlockHeaderSerializer.parseBytes(bytes.tail)
      case ADProofs.`modifierTypeId` =>
        ADProofSerializer.parseBytes(bytes.tail)
      case Payload.`modifierTypeId` =>
        EncryBlockPayloadSerializer.parseBytes(bytes.tail)
      case m =>
        Failure(new Exception(s"Deserialization for unknown type byte: $m"))
    }
}