package encry.network

import HeaderProto.HeaderProtoMessage
import PayloadProto.PayloadProtoMessage
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.history.{HeaderUtils, PayloadUtils}
import encry.settings.EncryAppSettings
import encry.view.history.History
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Header, HeaderProtoSerializer, Payload, PayloadProtoSerializer}
import org.encryfoundation.common.utils.TaggedTypes.ModifierTypeId
import cats.syntax.either._
import encry.modifiers.history.HeaderUtils.PreSemanticValidationException
import org.encryfoundation.common.validation.ValidationResult

import scala.util.{Failure, Try}

object ModifiersToNetworkUtils extends StrictLogging {

  def toProto(modifier: PersistentModifier): Array[Byte] = modifier match {
    case m: Header  => HeaderProtoSerializer.toProto(m).toByteArray
    case m: Payload => PayloadProtoSerializer.toProto(m).toByteArray
    case m          => throw new RuntimeException(s"Try to serialize unknown modifier: $m to proto.")
  }

  def fromProto(modType: ModifierTypeId, bytes: Array[Byte]): Try[PersistentModifier] =
    Try(modType match {
      case Header.modifierTypeId  => HeaderProtoSerializer.fromProto(HeaderProtoMessage.parseFrom(bytes))
      case Payload.modifierTypeId => PayloadProtoSerializer.fromProto(PayloadProtoMessage.parseFrom(bytes))
      case m                      => Failure(new RuntimeException(s"Try to deserialize unknown modifier: $m from proto."))
    }).flatten

  def isSyntacticallyValid(modifier: PersistentModifier, modifierIdSize: Int) = modifier match {
    case h: Header  => HeaderUtils.syntacticallyValidity(h, modifierIdSize)
    case p: Payload => PayloadUtils.syntacticallyValidity(p, modifierIdSize)
    case _          => ValidationResult.Valid
  }

  def isPreSemanticValidation(modifier: PersistentModifier,
                              history: History,
                              settings: EncryAppSettings): Either[PreSemanticValidationException, Unit] =
    modifier match {
      case h: Header => HeaderUtils.preSemanticValidation(h, history, settings)
      case _         => ().asRight[PreSemanticValidationException]
    }
}
