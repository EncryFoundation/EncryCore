package encry.view.mempool

import TransactionProto.TransactionProtoMessage
import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.network.BlackList.BanReason.{CorruptedSerializedBytes, SyntacticallyInvalidTransaction}
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.BanPeer
import encry.nvg.ModifiersValidator.InvalidModifierBytes
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.mempool.MemoryPool.NewTransaction
import encry.view.mempool.TransactionsValidator.{InvalidTransaction, ModifiersForValidating}
import org.encryfoundation.common.modifiers.mempool.transaction.{Transaction, TransactionProtoSerializer}
import org.encryfoundation.common.network.BasicMessagesRepo.ModifiersNetworkMessage
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

import scala.util.{Failure, Success, Try}

class TransactionsValidator(settings: EncryAppSettings,
                            memPool: ActorRef,
                            networkTimeProvider: NetworkTimeProvider)
    extends Actor
    with StrictLogging {

  override def receive(): Receive = {
    case DataFromPeer(ModifiersNetworkMessage(data), remote) if data._1 == Transaction.modifierTypeId =>
       data._2.foreach {
        case (id, bytes) =>
          Try(TransactionProtoSerializer.fromProto(TransactionProtoMessage.parseFrom(bytes))).flatten match {
            case Success(tx) if tx.semanticValidity.isSuccess => memPool ! NewTransaction(tx)
            case Success(tx) =>
              logger.info(s"Transaction with id: ${tx.encodedId} invalid cause of: ${tx.semanticValidity}.")
              context.parent ! BanPeer(remote, SyntacticallyInvalidTransaction)
              context.parent ! InvalidTransaction(id)
            case Failure(ex) =>
              context.parent ! BanPeer(remote, CorruptedSerializedBytes)
              context.parent ! InvalidModifierBytes(id)
              logger.info(s"Received modifier from $remote can't be parsed cause of: ${ex.getMessage}.")
          }
       }
  }
}

object TransactionsValidator {

  final case class ModifiersForValidating(remote: ConnectedPeer,
                                          typeId: ModifierTypeId,
                                          modifiers: Map[ModifierId, Array[Byte]])

  final case class InvalidTransaction(ids: ModifierId) extends AnyVal

  def props(settings: EncryAppSettings, memPool: ActorRef, ntp: NetworkTimeProvider): Props =
    Props(new TransactionsValidator(settings, memPool, ntp))
}
