package encry.network

import HeaderProto.HeaderProtoMessage
import PayloadProto.PayloadProtoMessage
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.history.{HeaderUtils, PayloadUtils}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Header, HeaderProtoSerializer, Payload, PayloadProtoSerializer}
import org.encryfoundation.common.modifiers.mempool.transaction.{Transaction, TransactionProtoSerializer}
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ModifierId, ModifierTypeId}
import scorex.crypto.hash.Digest32

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, _}
import cats.syntax.traverse._
import cats.instances.future._
import cats.instances.list._

object ModifiersToNetworkUtils extends StrictLogging {

  object PayloadProtoSerializerTest {

    def toProto(payload: Payload): PayloadProtoMessage = PayloadProtoMessage()
      .withHeaderId(ByteString.copyFrom(payload.headerId))
      .withTxs(payload.txs.map(_.toTransactionProto))

    def fromProto(payloadProtoMessage: PayloadProtoMessage): Try[Payload] = Try {
      val transactionsFuture = payloadProtoMessage.txs.toList.map(tx => Future(TransactionProtoSerializer.fromProto(tx).get)).sequence[Future, Transaction]
      val transactions = Await.result(transactionsFuture, 1 minutes)
      Payload(ModifierId @@ payloadProtoMessage.headerId.toByteArray, transactions)
    }
  }

  def toProto(modifier: PersistentModifier): Array[Byte] = modifier match {
    case m: Header   => HeaderProtoSerializer.toProto(m).toByteArray
    case m: Payload  => PayloadProtoSerializer.toProto(m).toByteArray
    case m           => throw new RuntimeException(s"Try to serialize unknown modifier: $m to proto.")
  }

  def fromProto(modType: ModifierTypeId, bytes: Array[Byte]): Try[PersistentModifier] = Try(modType match {
    case Header.modifierTypeId   => HeaderProtoSerializer.fromProto(HeaderProtoMessage.parseFrom(bytes))
    case Payload.modifierTypeId  => PayloadProtoSerializer.fromProto(PayloadProtoMessage.parseFrom(bytes))
    case m                       => Failure(new RuntimeException(s"Try to deserialize unknown modifier: $m from proto."))
  }).flatten

  def isSyntacticallyValid(modifier: PersistentModifier): Boolean = modifier match {
    case h: Header  => HeaderUtils.syntacticallyValidity(h).isSuccess
    case p: Payload => PayloadUtils.syntacticallyValidity(p).isSuccess
    case _          => true
  }
}