package encry.network

import BlockProto.BlockProtoMessage.AdProofsProtoMessage
import HeaderProto.HeaderProtoMessage
import PayloadProto.PayloadProtoMessage
import encry.modifiers.history.{HeaderUtils, PayloadUtils}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ADProofs, ADProofsProtoSerializer, Header, HeaderProtoSerializer, Payload, PayloadProtoSerializer}
import org.encryfoundation.common.utils.TaggedTypes.ModifierTypeId
import scala.util.{Failure, Try}

object ModifiersToNetworkUtils {

  def toProto(modifier: PersistentModifier): Array[Byte] = modifier match {
    case m: Header   => HeaderProtoSerializer.toProto(m).toByteArray
    case m: ADProofs => ADProofsProtoSerializer.toProto(m).toByteArray
    case m: Payload  => PayloadProtoSerializer.toProto(m).toByteArray
    case m           => throw new RuntimeException(s"Try to serialize unknown modifier: $m to proto.")
  }

  def fromProto(modType: ModifierTypeId, bytes: Array[Byte]): Try[PersistentModifier] = modType match {
    case Header.modifierTypeId   => HeaderProtoSerializer.fromProto(HeaderProtoMessage.parseFrom(bytes))
    case ADProofs.modifierTypeId => Try(ADProofsProtoSerializer.fromProto(AdProofsProtoMessage.parseFrom(bytes)))
    case Payload.modifierTypeId  => PayloadProtoSerializer.fromProto(PayloadProtoMessage.parseFrom(bytes))
    case m                       => Failure(new RuntimeException(s"Try to deserialize unknown modifier: $m from proto."))
  }

  def isSyntacticallyValid(modifier: PersistentModifier): Boolean = modifier match {
    case h: Header => HeaderUtils.syntacticallyValidity(h).isSuccess
    case p: Payload => PayloadUtils.syntacticallyValidity(p).isSuccess
    case _ => true
  }
}