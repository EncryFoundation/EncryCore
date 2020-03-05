package encry.view.mempool

import TransactionProto.TransactionProtoMessage
import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.network.BlackList.BanReason.{CorruptedSerializedBytes, SyntacticallyInvalidTransaction}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.BanPeer
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.mempool.MemoryPool.NewTransaction
import encry.view.mempool.TransactionsValidator.{InvalidModifier, ModifiersForValidating}
import org.encryfoundation.common.modifiers.mempool.transaction.{Transaction, TransactionProtoSerializer}
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import scala.util.{Failure, Success, Try}

class TransactionsValidator(settings: EncryAppSettings,
                            memPool: ActorRef,
                            networkTimeProvider: NetworkTimeProvider)
    extends Actor
    with StrictLogging {

  override def receive(): Receive = {
    case ModifiersForValidating(remote, typeId, filteredModifiers) =>
      typeId match {
        case Transaction.modifierTypeId =>
          filteredModifiers.foreach {
            case (id, bytes) =>
              Try(TransactionProtoSerializer.fromProto(TransactionProtoMessage.parseFrom(bytes))).flatten match {
                case Success(tx) if tx.semanticValidity.isSuccess => memPool ! NewTransaction(tx)
                case Success(tx) =>
                  logger.info(s"Transaction with id: ${tx.encodedId} invalid cause of: ${tx.semanticValidity}.")
                  context.parent ! BanPeer(remote.socketAddress, SyntacticallyInvalidTransaction)
                  context.parent ! InvalidModifier(id)
                case Failure(ex) =>
                  context.parent ! BanPeer(remote.socketAddress, CorruptedSerializedBytes)
                  context.parent ! InvalidModifier(id)
                  logger.info(s"Received modifier from $remote can't be parsed cause of: ${ex.getMessage}.")
              }
          }
      }
  }

}

object TransactionsValidator {

  final case class ModifiersForValidating(remote: ConnectedPeer,
                                          typeId: ModifierTypeId,
                                          modifiers: Map[ModifierId, Array[Byte]])

  final case class InvalidModifier(ids: ModifierId) extends AnyVal

  def props(settings: EncryAppSettings, memPool: ActorRef, ntp: NetworkTimeProvider): Props =
    Props(new TransactionsValidator(settings, memPool, ntp))
}
