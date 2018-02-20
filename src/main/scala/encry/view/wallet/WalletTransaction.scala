package encry.view.wallet

import com.google.common.primitives.Bytes
import encry.modifiers.mempool._
import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.util.Try

case class WalletTransaction(tx: EncryBaseTransaction) extends BytesSerializable {
  override type M = WalletTransaction

  override def serializer: Serializer[M] = WalletTransactionSerializer
}

object WalletTransactionSerializer extends Serializer[WalletTransaction] {

  override def toBytes(obj: WalletTransaction): Array[Byte] =
    obj.tx match {
      case tx: PaymentTransaction => (0: Byte) +: PaymentTransactionSerializer.toBytes(tx)
      case tx: AddPubKeyInfoTransaction => (1: Byte) +: AddPubKeyInfoTransactionSerializer.toBytes(tx)
      case _ => Array.empty[Byte]
    }

  override def parseBytes(bytes: Array[Byte]): Try[WalletTransaction] = Try {
   bytes.head match {
      case 0 => WalletTransaction(PaymentTransactionSerializer.parseBytes(bytes.slice(1, bytes.length)).get)
      case 1 => WalletTransaction(AddPubKeyInfoTransactionSerializer.parseBytes(bytes.slice(1, bytes.length)).get)
      case _ => throw new Error("Unsupported tx type")
    }
  }
}
