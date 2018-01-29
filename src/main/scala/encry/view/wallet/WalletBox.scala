package encry.view.wallet

import com.google.common.primitives.Bytes
import encry.modifiers.state.box._
import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.util.Try

case class WalletBox(box: AssetBox) extends BytesSerializable{

  override type M = WalletBox

  override def serializer: Serializer[M] = WalletBoxSerializer
}

object WalletBoxSerializer extends Serializer[WalletBox]{

  override def toBytes(obj: WalletBox): Array[Byte] =
    Bytes.concat(
      obj.box.serializer.toBytes(obj.box)
    )

  override def parseBytes(bytes: Array[Byte]): Try[WalletBox] = Try {
    val box = AssetBoxSerializer.parseBytes(bytes).get
    WalletBox(box)
  }
}
