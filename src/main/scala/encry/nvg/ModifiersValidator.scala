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
  ModifierIdInTheNetworkMessageIsNotTheSameAsIdOfModifierInThisMessage,
  PreSemanticInvalidModifier,
  SyntacticallyInvalidPersistentModifier
}
import encry.network.PeersKeeper.BanPeer
import encry.nvg.ModifiersValidator.{ InvalidModifierBytes, ModifierForValidation, ValidatedModifier }
import encry.nvg.NodeViewHolder.SyntacticallyFailedModification
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.ValidatedModifierFromNetwork
import encry.view.history.HistoryReader
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ Header, HeaderProtoSerializer, Payload, PayloadProtoSerializer }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ ModifierId, ModifierTypeId }

import scala.util.Try

class ModifiersValidator(
  nodeViewHolderRef: ActorRef,
  intermediaryNVH: ActorRef,
  settings: EncryAppSettings
) extends Actor
    with StrictLogging {

  override def receive: Receive = {
    case ModifierForValidation(reader, id, modifierTypeId, modifierBytes, remote) if !reader.isModifierDefined(id) =>
      ModifiersValidator.fromProto(modifierTypeId, modifierBytes) match {
        case Left(error) =>
          logger.info(s"Modifier ${Algos.encode(id)} is incorrect cause: ${error.getMessage}.")
          intermediaryNVH ! BanPeer(remote, CorruptedSerializedBytes)
          intermediaryNVH ! InvalidModifierBytes(id)
        case Right(modifier) =>
          val preSemanticValidation: Either[PreSemanticValidationException, Unit] =
            ModifiersValidator.isPreSemanticValid(modifier, reader, settings)
          val syntacticValidation: Boolean =
            ModifiersValidator.isSyntacticallyValid(modifier, settings.constants.ModifierIdSize)
          if (preSemanticValidation.isRight && syntacticValidation) {
            if (modifier.id.sameElements(id)) {
              logger.debug(s"Modifier ${modifier.encodedId} is valid.")
              intermediaryNVH ! ValidatedModifierFromNetwork(modifierTypeId)
              nodeViewHolderRef ! ValidatedModifier(modifier)
            } else {
              logger.info(s"Modifier ${modifier.encodedId} should have ${Algos.encode(id)} id!")
              intermediaryNVH ! BanPeer(remote, ModifierIdInTheNetworkMessageIsNotTheSameAsIdOfModifierInThisMessage)
              intermediaryNVH ! SyntacticallyFailedModification(modifier, List.empty)
            }
          } else if (!syntacticValidation) {
            logger.info(s"Modifier ${modifier.encodedId} is syntactically invalid.")
            intermediaryNVH ! BanPeer(remote, SyntacticallyInvalidPersistentModifier)
            intermediaryNVH ! SyntacticallyFailedModification(modifier, List.empty)
          } else
            preSemanticValidation.leftMap {
              case IllegalHeight(error) =>
                logger.info(s"Modifier ${modifier.encodedId} is invalid cause: $error.")
                intermediaryNVH ! BanPeer(remote, PreSemanticInvalidModifier(error))
                intermediaryNVH ! SyntacticallyFailedModification(modifier, List.empty)
            }
      }
    case m: ModifierForValidation =>
      logger.info(s"Got modifier ${Algos.encode(m.modifierId)} but this mod is already in history.")
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
      case Header.modifierTypeId  => Try(HeaderProtoSerializer.fromProto(HeaderProtoMessage.parseFrom(bytes))).flatten
      case Payload.modifierTypeId => Try(PayloadProtoSerializer.fromProto(PayloadProtoMessage.parseFrom(bytes))).flatten
    })

  def isSyntacticallyValid(
    modifier: PersistentModifier,
    modifierIdSize: Int
  ): Boolean = modifier match {
    case h: Header  => HeaderUtils.syntacticallyValidity(h, modifierIdSize).isSuccess
    case p: Payload => PayloadUtils.syntacticallyValidity(p, modifierIdSize).isSuccess
  }

  def props(nodeViewHolderRef: ActorRef, intermediaryNVH: ActorRef, settings: EncryAppSettings): Props =
    Props(new ModifiersValidator(nodeViewHolderRef, intermediaryNVH, settings))
}
