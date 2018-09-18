package encry.modifiers.history

import encry.modifiers.EncryPersistentModifier
import org.encryfoundation.common.serialization.Serializer
import scala.util.{Failure, Try}

object HistoryModifierSerializer extends Serializer[EncryPersistentModifier] {

  override def toBytes(obj: EncryPersistentModifier): Array[Byte] = obj match {
    case m: Header => Header.modifierTypeId +: HeaderSerializer.toBytes(m)
    case m: ADProofs => ADProofs.modifierTypeId +: ADProofSerializer.toBytes(m)
    case m: Payload => Payload.modifierTypeId +: PayloadSerializer.toBytes(m)
    case m => throw new Exception(s"Serialization for unknown modifier: $m")
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryPersistentModifier] =
    Try(bytes.head).flatMap {
      case Header.`modifierTypeId` => HeaderSerializer.parseBytes(bytes.tail)
      case ADProofs.`modifierTypeId` => ADProofSerializer.parseBytes(bytes.tail)
      case Payload.`modifierTypeId` => PayloadSerializer.parseBytes(bytes.tail)
      case m => Failure(new Exception(s"Deserialization for unknown type byte: $m"))
    }
}