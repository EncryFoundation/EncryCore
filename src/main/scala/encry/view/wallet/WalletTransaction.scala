package encry.view.wallet

import com.google.common.primitives.Bytes
import encry.modifiers.mempool.{PaymentTransaction, PaymentTransactionSerializer}
import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.util.Try

case class WalletTransaction(tx: PaymentTransaction) extends BytesSerializable {
  override type M = WalletTransaction

  override def serializer: Serializer[M] = WalletTransactionSerializer
}

object WalletTransactionSerializer extends Serializer[WalletTransaction] {

  override def toBytes(obj: WalletTransaction): Array[Byte] =
    Bytes.concat(
      PaymentTransactionSerializer.toBytes(obj.tx)
    )

  override def parseBytes(bytes: Array[Byte]): Try[WalletTransaction] = Try {
    val tx = PaymentTransactionSerializer.parseBytes(bytes).get
    WalletTransaction(tx)
  }
}
