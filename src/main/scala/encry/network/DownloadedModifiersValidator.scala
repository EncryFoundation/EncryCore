package encry.network

import TransactionProto.TransactionProtoMessage
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.network.BlackList.BanReason._
import encry.network.DownloadedModifiersValidator.{InvalidModifier, ModifiersForValidating}
import encry.network.NodeViewSynchronizer.ReceivableMessages.UpdatedHistory
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.BanPeer
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.ValidatedModifierFromNetwork
import encry.view.history.History
import encry.view.NodeViewHolder.ReceivableMessages.ModifierFromRemote
import encry.view.mempool.MemoryPool.NewTransaction
import org.encryfoundation.common.modifiers.mempool.transaction.{Transaction, TransactionProtoSerializer}
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import scala.util.{Failure, Success, Try}

class DownloadedModifiersValidator(modifierIdSize: Int,
                                   nodeViewHolder: ActorRef,
                                   peersKeeper: ActorRef,
                                   nodeViewSync: ActorRef,
                                   memoryPoolRef: ActorRef,
                                   influxRef: Option[ActorRef]) extends Actor with StrictLogging {

  override def receive: Receive = {
    case UpdatedHistory(historyReader) => context.become(workingCycle(historyReader))
    case msg => logger.info(s"Got $msg on DownloadedModifiersValidator")
  }

  def workingCycle(history: History): Receive = {
    case ModifiersForValidating(remote, typeId, filteredModifiers) if typeId != Transaction.modifierTypeId =>
      filteredModifiers.foreach { case (id, bytes) =>
        ModifiersToNetworkUtils.fromProto(typeId, bytes) match {
          case Success(modifier) if ModifiersToNetworkUtils.isSyntacticallyValid(modifier, modifierIdSize) =>
            logger.debug(s"Modifier: ${modifier.encodedId} after testApplicable is correct. " +
              s"Sending validated modifier to NodeViewHolder")
            influxRef.foreach(_ ! ValidatedModifierFromNetwork(typeId))
            nodeViewHolder ! ModifierFromRemote(modifier)
          case Success(modifier) =>
            logger.info(s"Modifier with id: ${modifier.encodedId} of type: $typeId invalid cause of: isSyntacticallyValid = false")
            peersKeeper ! BanPeer(remote, SyntacticallyInvalidPersistentModifier)
            nodeViewSync ! InvalidModifier(id)
          case Failure(ex) =>
            peersKeeper ! BanPeer(remote, CorruptedSerializedBytes)
            logger.info(s"Received modifier from $remote can't be parsed cause of: ${ex.getMessage}.")
            nodeViewSync ! InvalidModifier(id)
        }
      }

    case ModifiersForValidating(remote, typeId, filteredModifiers) => typeId match {
      case Transaction.modifierTypeId => filteredModifiers
        .foreach { case (id, bytes) =>
          Try(TransactionProtoSerializer.fromProto(TransactionProtoMessage.parseFrom(bytes))).flatten match {
            case Success(tx) if tx.semanticValidity.isSuccess => memoryPoolRef ! NewTransaction(tx)
            case Success(tx) =>
              logger.info(s"Payload with id: ${tx.encodedId} invalid cause of: ${tx.semanticValidity}.")
              context.parent ! BanPeer(remote, SyntacticallyInvalidTransaction)
              nodeViewSync ! InvalidModifier(id)
            case Failure(ex) =>
              context.parent ! BanPeer(remote, CorruptedSerializedBytes)
              nodeViewSync ! InvalidModifier(id)
              logger.info(s"Received modifier from $remote can't be parsed cause of: ${ex.getMessage}.")
          }
        }
    }
    case UpdatedHistory(historyReader) => context.become(workingCycle(historyReader))
    case msg => logger.info(s"Got $msg on DownloadedModifiersValidator")
  }

}

object DownloadedModifiersValidator {

  final case class ModifiersForValidating(remote: ConnectedPeer,
                                          typeId: ModifierTypeId,
                                          modifiers: Map[ModifierId, Array[Byte]])

  final case class InvalidModifier(ids: ModifierId) extends AnyVal

  def props(modifierIdSize: Int,
            nodeViewHolder: ActorRef,
            peersKeeper: ActorRef,
            nodeViewSync: ActorRef,
            memoryPoolRef: ActorRef,
            influxRef: Option[ActorRef]): Props =
    Props(new DownloadedModifiersValidator(modifierIdSize, nodeViewHolder, peersKeeper, nodeViewSync, memoryPoolRef, influxRef))

  class DownloadedModifiersValidatorPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case UpdatedHistory(_) => 0
        case PoisonPill => 2
        case _ => 1
      })

}