package encry.nvg

import java.net.InetSocketAddress

import HeaderProto.HeaderProtoMessage
import PayloadProto.PayloadProtoMessage
import akka.actor.{ Actor, ActorRef, Props }
import cats.syntax.either._
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.history.HeaderUtils.{ IllegalHeight, PreSemanticValidationException }
import encry.modifiers.history.{ HeaderUtils, PayloadUtils }
import encry.network.BlackList.BanReason.{
  CorruptedSerializedBytes,
  PreSemanticInvalidModifier,
  SyntacticallyInvalidPersistentModifier
}
import encry.network.NodeViewSynchronizer.ReceivableMessages.SyntacticallyFailedModification
import encry.network.PeersKeeper.BanPeer
import encry.nvg.ModifiersValidator.{ InvalidModifierBytes, ModifierForValidation, ValidatedModifier }
import encry.settings.EncryAppSettings
import encry.view.history.HistoryReader
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ Header, HeaderProtoSerializer, Payload, PayloadProtoSerializer }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ ModifierId, ModifierTypeId }

class ModifiersValidator(nodeViewHolderRef: ActorRef, settings: EncryAppSettings) extends Actor with StrictLogging {

  override def receive: Receive = {
    case ModifierForValidation(reader, modifierId, modifierTypeId, modifierBytes, remote) =>
      fromProto(modifierTypeId, modifierBytes) match {
        case Left(error) =>
          logger.info(s"Modifier ${Algos.encode(modifierId)} is incorrect cause: ${error.getMessage}.")
          context.parent ! BanPeer(remote, CorruptedSerializedBytes)
          context.parent ! InvalidModifierBytes(modifierId)
        case Right(modifier) =>
          val preSemanticValidation: Either[PreSemanticValidationException, Unit] =
            isPreSemanticValid(modifier, reader, settings)
          val syntacticValidation: Boolean =
            isSyntacticallyValid(modifier, settings.constants.ModifierIdSize)
          if (preSemanticValidation.isRight && syntacticValidation) {
            logger.debug(s"Modifier ${modifier.encodedId} is valid.")
            nodeViewHolderRef ! ValidatedModifier(modifier)
          } else if (!syntacticValidation) {
            logger.info(s"Modifier ${modifier.encodedId} is syntactically invalid.")
            context.parent ! BanPeer(remote, SyntacticallyInvalidPersistentModifier)
            context.parent ! SyntacticallyFailedModification(modifier, List.empty)
          } else
            preSemanticValidation.leftMap {
              case IllegalHeight(error) =>
                logger.info(s"Modifier ${modifier.encodedId} is invalid cause: $error.")
                context.parent ! BanPeer(remote, PreSemanticInvalidModifier(error))
                context.parent ! SyntacticallyFailedModification(modifier, List.empty)
            }
      }
  }

  private def isPreSemanticValid(
    modifier: PersistentModifier,
    historyReader: HistoryReader,
    settings: EncryAppSettings
  ): Either[PreSemanticValidationException, Unit] =
    modifier match {
      case header: Header =>
        val bestHeaderHeight: Int = historyReader.getBestHeaderHeight
        if (bestHeaderHeight - settings.constants.MaxRollbackDepth <= header.height) ().asRight
        else
          IllegalHeight(
            s"Height of received header is ${header.height}. Current best header height is $bestHeaderHeight. " +
              s"Max possible received header's height is ${bestHeaderHeight - settings.constants.MaxRollbackDepth}."
          ).asLeft
      case _: Payload => ().asRight[PreSemanticValidationException]
    }

  private def fromProto(
    modType: ModifierTypeId,
    bytes: Array[Byte]
  ): Either[Throwable, PersistentModifier] =
    Either.fromTry(modType match {
      case Header.modifierTypeId  => HeaderProtoSerializer.fromProto(HeaderProtoMessage.parseFrom(bytes))
      case Payload.modifierTypeId => PayloadProtoSerializer.fromProto(PayloadProtoMessage.parseFrom(bytes))
    })

  def isSyntacticallyValid(
    modifier: PersistentModifier,
    modifierIdSize: Int
  ): Boolean = modifier match {
    case h: Header  => HeaderUtils.syntacticallyValidity(h, modifierIdSize).isSuccess
    case p: Payload => PayloadUtils.syntacticallyValidity(p, modifierIdSize).isSuccess
  }
}

object ModifiersValidator {

  final case class ModifierForValidation(
    historyReader: HistoryReader,
    modifierId: ModifierId,
    modifierTypeId: ModifierTypeId,
    modifierBytes: Array[Byte],
    remote: InetSocketAddress
  )

  final case class ValidatedModifier(modifier: PersistentModifier) extends AnyVal

  final case class InvalidModifierBytes(id: ModifierId) extends AnyVal

  def props(nodeViewHolderRef: ActorRef, settings: EncryAppSettings): Props =
    Props(new ModifiersValidator(nodeViewHolderRef, settings))
}
