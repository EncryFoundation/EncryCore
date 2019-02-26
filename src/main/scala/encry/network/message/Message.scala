package encry.network.message

import com.google.common.primitives.{Bytes, Ints}
import encry.network.PeerConnectionHandler._
import org.encryfoundation.common.serialization.{BytesSerializable, Serializer}
import scorex.crypto.hash.Blake2b256
import scala.util.Try

case class Message(spec: NetworkMessage, source: Option[ConnectedPeer]) {

  def toProto = ???

  def fromProto = ???

}

//class MessageSerializer[Content] extends Serializer[Message[Content]] {

//  import Message.{ChecksumLength, MAGIC}
//
//  override def toBytes(obj: Message[Content]): Array[Byte] = {
//    val dataWithChecksum = if (obj.dataLength > 0) {
//      val checksum = Blake2b256.hash(obj.dataBytes).take(ChecksumLength)
//      Bytes.concat(checksum, obj.dataBytes)
//    } else obj.dataBytes //empty array
//
//    MAGIC ++ Array(obj.spec.messageCode) ++ Ints.toByteArray(obj.dataLength) ++ dataWithChecksum
//  }
//
//  override def parseBytes(bytes: Array[Byte]): Try[Message[Content]] = ???
//}

object Message {

  //todo Am i need this?
  val MAGIC: Array[Byte] = Array[Byte](0x12: Byte, 0x34: Byte, 0x56: Byte, 0x78: Byte)

  val MagicLength: Int = MAGIC.length

  val ChecksumLength: Int = 4
}
