package encry.view.wallet

import encry.modifiers.{BytesSerializable, Serializer}
import encry.modifiers.mempool._

import scala.util.Try

case class WalletTransaction(tx: EncryBaseTransaction) extends BytesSerializable {
  override type M = WalletTransaction

  override def serializer: Serializer[M] = WalletTransactionSerializer
}

object WalletTransactionSerializer extends Serializer[WalletTransaction] {

  override def toBytes(obj: WalletTransaction): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[WalletTransaction] = ???
}
