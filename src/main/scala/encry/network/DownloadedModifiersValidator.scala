package encry.network

import TransactionProto.TransactionProtoMessage
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.network.BlackList.BanReason._
import encry.network.DownloadedModifiersValidator.{InvalidModifiers, ModifiersForValidating}
import encry.network.NodeViewSynchronizer.ReceivableMessages.UpdatedHistory
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.BanPeer
import encry.settings.EncryAppSettings
import encry.view.NodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.EncryHistory
import encry.view.mempool.Mempool.TransactionsFromRemote
import org.encryfoundation.common.modifiers.mempool.transaction.{Transaction, TransactionProtoSerializer}
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.utils.Algos
import scala.util.{Failure, Success, Try}

class DownloadedModifiersValidator(settings: EncryAppSettings,
                                   nodeViewHolder: ActorRef,
                                   peersKeeper: ActorRef,
                                   nodeViewSync: ActorRef,
                                   memoryPoolRef: ActorRef) extends Actor with StrictLogging {

  override def receive: Receive = {
    case UpdatedHistory(historyReader) => context.become(workingCycle(historyReader))
    case msg => logger.info(s"Got $msg on DownloadedModifiersValidator")
  }

  def workingCycle(history: EncryHistory): Receive = {
    case ModifiersForValidating(remote, typeId, filteredModifiers) if typeId != Transaction.modifierTypeId =>
      val modifiers = filteredModifiers.foldLeft(Seq.empty[PersistentModifier], Seq.empty[ModifierId]) {
        case ((modsColl, forRemove), (id, bytes)) => ModifiersToNetworkUtils.fromProto(typeId, bytes) match {
          case Success(modifier) if ModifiersToNetworkUtils.isSyntacticallyValid(modifier) =>
            logger.debug(s"Modifier: ${modifier.encodedId} after testApplicable is correct.")
            (modsColl :+ modifier, forRemove)
          case Success(modifier) =>
            logger.info(s"Modifier with id: ${modifier.encodedId} of type: $typeId invalid cause of: isSyntacticallyValid = false")
            peersKeeper ! BanPeer(remote, SyntacticallyInvalidPersistentModifier)
            (modsColl, forRemove :+ id)
          case Failure(ex) =>
            peersKeeper ! BanPeer(remote, CorruptedSerializedBytes)
            logger.info(s"Received modifier from $remote can't be parsed cause of: ${ex.getMessage}.")
            (modsColl, forRemove :+ id)
        }
      }

      if (modifiers._1.nonEmpty) {
        logger.debug(s"Sending to node view holder parsed modifiers: ${modifiers._1.size}.")
        nodeViewHolder ! ModifiersFromRemote(modifiers._1)
      }
      if (modifiers._2.nonEmpty) {
        logger.debug(s"Sending to delivery manager invalid modifiers: ${modifiers._2.map(k => Algos.encode(k))}.")
        nodeViewSync ! InvalidModifiers(modifiers._2)
      }

    case ModifiersForValidating(remote, typeId, filteredModifiers) => typeId match {
      case Transaction.modifierTypeId =>
        val transactions: (Seq[Transaction], Seq[ModifierId]) = filteredModifiers
          .foldLeft(Seq.empty[Transaction], Seq.empty[ModifierId]) {
            case ((transactionsColl, forRemove), (id, bytes)) =>
              Try(TransactionProtoSerializer.fromProto(TransactionProtoMessage.parseFrom(bytes))).flatten match {
                case Success(tx) if tx.semanticValidity.isSuccess => (transactionsColl :+ tx, forRemove)
                case Success(tx) =>
                  logger.info(s"Payload with id: ${tx.encodedId} invalid cause of: ${tx.semanticValidity}.")
                  context.parent ! BanPeer(remote, SyntacticallyInvalidTransaction)
                  (transactionsColl, forRemove :+ id)
                case Failure(ex) =>
                  context.parent ! BanPeer(remote, CorruptedSerializedBytes)
                  logger.info(s"Received modifier from $remote can't be parsed cause of: ${ex.getMessage}.")
                  (transactionsColl, forRemove :+ id)
              }
          }

        if (transactions._1.nonEmpty) {
          logger.debug(s"Sending to node mempool parsed transactions: ${transactions._1.map(_.encodedId)}.")
          memoryPoolRef ! TransactionsFromRemote(transactions._1)
        }
        if (transactions._2.nonEmpty) {
          logger.debug(s"Sending to delivery manager invalid modifiers: ${transactions._2.map(k => Algos.encode(k))}.")
          nodeViewSync ! InvalidModifiers(transactions._2)
        }
    }
    case UpdatedHistory(historyReader) => context.become(workingCycle(historyReader))
    case msg => logger.info(s"Got $msg on DownloadedModifiersValidator")
  }

}

object DownloadedModifiersValidator {

  final case class ModifiersForValidating(remote: ConnectedPeer,
                                          typeId: ModifierTypeId,
                                          modifiers: Seq[(ModifierId, Array[Byte])])

  final case class InvalidModifiers(ids: Seq[ModifierId])

  def props(settings: EncryAppSettings,
            nodeViewHolder: ActorRef,
            peersKeeper: ActorRef,
            nodeViewSync: ActorRef,
            memoryPoolRef: ActorRef): Props =
    Props(new DownloadedModifiersValidator(settings, nodeViewHolder, peersKeeper, nodeViewSync, memoryPoolRef))

  class DownloadedModifiersValidatorPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case UpdatedHistory(_) => 0
        case PoisonPill        => 2
        case _                 => 1
      })
}