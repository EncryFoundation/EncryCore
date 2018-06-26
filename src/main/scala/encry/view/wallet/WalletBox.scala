package encry.view.wallet

import encry.modifiers.serialization.{BytesSerializable, Serializer}
import encry.modifiers.state.box._

import scala.util.Try

case class WalletBox(box: EncryBaseBox) extends BytesSerializable{

  override type M = WalletBox

  override def serializer: Serializer[M] = WalletBoxSerializer
}

object WalletBoxSerializer extends Serializer[WalletBox]{

  override def toBytes(obj: WalletBox): Array[Byte] = obj.bytes

  override def parseBytes(bytes: Array[Byte]): Try[WalletBox] = ???
}
