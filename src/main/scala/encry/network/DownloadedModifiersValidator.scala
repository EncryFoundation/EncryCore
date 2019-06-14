package encry.network

import HeaderProto.HeaderProtoMessage
import PayloadProto.PayloadProtoMessage
import TransactionProto.TransactionProtoMessage
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.network.BlackList.{InvalidModifierFromNetwork, SemanticallyInvalidModifier, SyntacticallyInvalidModifier}
import encry.network.DownloadedModifiersValidator.{ModifiersForValidating, ModifiersIdsForRemove}
import encry.network.NodeViewSynchronizer.ReceivableMessages.UpdatedHistory
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.BanPeer
import encry.settings.EncryAppSettings
import encry.view.NodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.EncryHistory
import encry.view.mempool.Mempool.TransactionsFromRemote
import org.encryfoundation.common.modifiers.history.{Header, HeaderProtoSerializer, Payload, PayloadProtoSerializer}
import org.encryfoundation.common.modifiers.mempool.transaction.{Transaction, TransactionProtoSerializer}
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import org.encryfoundation.common.validation.RecoverableModifierError
import encry.modifiers.history.{HeaderUtils => HU, PayloadUtils => PU}
import org.encryfoundation.common.utils.Algos
import scala.util.{Failure, Success}

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
    case ModifiersForValidating(remote, typeId, filteredModifiers) => typeId match {
      case Payload.modifierTypeId =>
        val payloads: (Seq[Payload], Seq[ModifierId]) = filteredModifiers
          .foldLeft(Seq.empty[Payload], Seq.empty[ModifierId]) { case ((payloadsColl, forRemove), (id, bytes)) =>
            PayloadProtoSerializer.fromProto(PayloadProtoMessage.parseFrom(bytes)) match {
              case Success(payload) if PU.syntacticallyValidity(payload).isSuccess => history.testApplicable(payload) match {
                case Failure(ex: RecoverableModifierError) =>
                  logger.debug(s"payload: ${payload.encodedId} after testApplicable has: ${ex.getMessage}. But this is " +
                    s"RecoverableModifierError so continue working with this modifier.")
                  (payloadsColl :+ payload, forRemove)
                case Failure(ex) =>
                  logger.info(s"payload: ${payload.encodedId} after testApplicable has: ${ex.getMessage}. This is " +
                    s"unhandled exception so reject this modifier and ban peer: $remote")
                  peersKeeper ! BanPeer(remote, SemanticallyInvalidModifier)
                  (payloadsColl, forRemove :+ id)
                case Success(_) =>
                  logger.debug(s"Header: ${payload.encodedId} after testApplicable is correct.")
                  (payloadsColl :+ payload, forRemove)
              }
              case Success(payload) =>
                logger.info(s"Payload with id: ${payload.encodedId} invalid cause of: " +
                  s"${PU.syntacticallyValidity(payload)}")
                peersKeeper ! BanPeer(remote, SyntacticallyInvalidModifier)
                (payloadsColl, forRemove :+ id)
              case Failure(ex) =>
                peersKeeper ! BanPeer(remote, SyntacticallyInvalidModifier)
                logger.info(s"Received payload from $remote can't be parsed cause of: ${ex.getMessage}.")
                (payloadsColl, forRemove :+ id)
            }
          }
        logger.debug(s"Sending to node view holder parsed payloads: ${payloads._1.map(_.encodedId)}.")
        if (payloads._1.nonEmpty) nodeViewHolder ! ModifiersFromRemote(payloads._1)
        logger.info(s"Sending to delivery manager invalid modifiers: ${payloads._2.map(k => Algos.encode(k))}.")
        if (payloads._2.nonEmpty) nodeViewSync ! ModifiersIdsForRemove(payloads._2)

      case Header.modifierTypeId =>
        val headers = filteredModifiers
          .foldLeft(Seq.empty[Header], Seq.empty[ModifierId]) { case ((headersCollection, forRemove), (id, bytes)) =>
            HeaderProtoSerializer.fromProto(HeaderProtoMessage.parseFrom(bytes)) match {
              case Success(value) if HU.syntacticallyValidity(value).isSuccess => history.testApplicable(value) match {
                case Failure(ex: RecoverableModifierError) =>
                  logger.debug(s"Header: ${value.encodedId} after testApplicable has: ${ex.getMessage}. But this is " +
                    s"RecoverableModifierError so continue working with this modifier.")
                  (headersCollection :+ value, forRemove)
                case Failure(ex) =>
                  logger.info(s"Header: ${value.encodedId} after testApplicable has: ${ex.getMessage}. This is " +
                    s"unhandled exception so reject this modifier and ban peer: $remote")
                  peersKeeper ! BanPeer(remote, SemanticallyInvalidModifier)
                  (headersCollection, forRemove :+ id)
                case Success(_) =>
                  logger.debug(s"Header: ${value.encodedId} after testApplicable is correct.")
                  (headersCollection :+ value, forRemove)
              }
              case Success(header) =>
                logger.info(s"Header with id: ${header.encodedId} invalid cause of:" +
                  s" ${HU.syntacticallyValidity(header)}. Ban peer: $remote")
                peersKeeper ! BanPeer(remote, SyntacticallyInvalidModifier)
                (headersCollection, forRemove :+ id)
              case Failure(ex) =>
                peersKeeper ! BanPeer(remote, InvalidModifierFromNetwork)
                logger.info(s"Received modifier from $remote can't be parsed cause of: ${ex.getMessage}.")
                (headersCollection, forRemove :+ id)
            }
          }
        logger.debug(s"Sending to node view holder parsed headers: ${headers._1.map(_.encodedId)}.")
        if (headers._1.nonEmpty) nodeViewHolder ! ModifiersFromRemote(headers._1)
        logger.info(s"Sending to delivery manager invalid modifiers: ${headers._2.map(k => Algos.encode(k))}.")
        if (headers._2.nonEmpty) nodeViewSync ! ModifiersIdsForRemove(headers._2)

      case Transaction.modifierTypeId =>
        val transactions: (Seq[Transaction], Seq[ModifierId]) = filteredModifiers
          .foldLeft(Seq.empty[Transaction], Seq.empty[ModifierId]) {
          case ((transactionsColl, forRemove), (id, bytes)) =>
            TransactionProtoSerializer.fromProto(TransactionProtoMessage.parseFrom(bytes)) match {
              case Success(tx) if tx.semanticValidity.isSuccess => (transactionsColl :+ tx, forRemove)
              case Success(tx) =>
                logger.info(s"Payload with id: ${tx.encodedId} invalid caze of: ${tx.semanticValidity}.")
                context.parent ! BanPeer(remote, SyntacticallyInvalidModifier)
                (transactionsColl, forRemove :+ id)
              case Failure(ex) =>
                context.parent ! BanPeer(remote, SyntacticallyInvalidModifier)
                logger.info(s"Received modifier from $remote can't be parsed cause of: ${ex.getMessage}.")
                (transactionsColl, forRemove :+ id)
            }
        }
        logger.debug(s"Sending to node mempool parsed transactions: ${transactions._1.map(_.encodedId)}.")
        if (transactions._1.nonEmpty) memoryPoolRef ! TransactionsFromRemote(transactions._1)
        logger.debug(s"Sending to delivery manager invalid modifiers: ${transactions._2.map(k => Algos.encode(k))}.")
        if (transactions._2.nonEmpty) nodeViewSync ! ModifiersIdsForRemove(transactions._2)
    }
    case UpdatedHistory(historyReader) => context.become(workingCycle(historyReader))
    case msg => logger.info(s"Got $msg on DownloadedModifiersValidator")
  }

}

object DownloadedModifiersValidator {

  final case class ModifiersForValidating(remote: ConnectedPeer,
                                          typeId: ModifierTypeId,
                                          modifiers: Seq[(ModifierId, Array[Byte])])

  final case class ModifiersIdsForRemove(ids: Seq[ModifierId])

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
        case PoisonPill => 2
        case otherwise => 1
      })
}