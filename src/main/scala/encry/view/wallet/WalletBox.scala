package encry.view.wallet

import com.google.common.primitives.Bytes
import encry.modifiers.state.box._
import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.util.Try

case class WalletBox(box: EncryBaseBox) extends BytesSerializable{

  override type M = WalletBox

  override def serializer: Serializer[M] = WalletBoxSerializer
}

object WalletBoxSerializer extends Serializer[WalletBox]{

  override def toBytes(obj: WalletBox): Array[Byte] =
    obj.box match {
      case bx: AssetBox => (0: Byte) +: AssetBoxSerializer.toBytes(bx)
      case bx: PubKeyInfoBox => (1: Byte) +: PubKeyInfoBoxSerializer.toBytes(bx)
      case _ => Array.empty[Byte]
    }

  override def parseBytes(bytes: Array[Byte]): Try[WalletBox] = Try {
    bytes.head match {
      case 0 => WalletBox(AssetBoxSerializer.parseBytes(bytes.slice(1, bytes.length)).get)
      case 1 => WalletBox(PubKeyInfoBoxSerializer.parseBytes(bytes.slice(1, bytes.length)).get)
      case _ => throw new Error("Unsupported box type")
    }
  }
}
