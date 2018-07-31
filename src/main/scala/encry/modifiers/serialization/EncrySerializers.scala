package encry.modifiers.serialization

import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.history.block.payload.{EncryBlockPayload, EncryBlockPayloadSerializer}
import encry.modifiers.history.{ADProofSerializer, ADProofs}
import encry.modifiers.mempool.{EncryTransaction, EncryTransactionSerializer}
import akka.serialization.{Serializer => AkkaSerializer}
import encry.utils.Logging
import scala.util.{Failure, Success}

case class EncryDeserializationException(th: Throwable) extends RuntimeException(th.getLocalizedMessage)

class EncryTxSerializer extends AkkaSerializer {
  override def identifier: Int = 41

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case tx: EncryTransaction => EncryTransactionSerializer.toBytes(tx)
    case _ => throw new IllegalArgumentException
  }

  override def includeManifest: Boolean = false

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef =
    EncryTransactionSerializer.parseBytes(bytes) match {
      case Success(tx) => tx
      case Failure(th) => throw EncryDeserializationException(th)
    }
}

class EncryPayloadSerializer extends AkkaSerializer {
  override def identifier: Int = 42

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case p: EncryBlockPayload => EncryBlockPayloadSerializer.toBytes(p)
    case _ => throw new IllegalArgumentException
  }

  override def includeManifest: Boolean = false

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef =
    EncryBlockPayloadSerializer.parseBytes(bytes) match {
      case Success(payload) => payload
      case Failure(th) => throw EncryDeserializationException(th)
    }
}

class EncryProofsSerializer extends AkkaSerializer {
  override def identifier: Int = 43

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case ad: ADProofs => ADProofSerializer.toBytes(ad)
    case _ => throw new IllegalArgumentException
  }

  override def includeManifest: Boolean = false

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef =
    ADProofSerializer.parseBytes(bytes) match {
      case Success(proofs) => proofs
      case Failure(th) => throw EncryDeserializationException(th)
    }
}

class EncryHeaderSerializer extends AkkaSerializer with Logging {
  override def identifier: Int = 44

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case h: EncryBlockHeader => EncryBlockHeaderSerializer.toBytes(h)
    case _ => throw new IllegalArgumentException
  }

  override def includeManifest: Boolean = false

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef =
    EncryBlockHeaderSerializer.parseBytes(bytes) match {
      case Success(header) => header
      case Failure(th) => throw EncryDeserializationException(th)
    }
}



