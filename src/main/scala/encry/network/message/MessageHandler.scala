package encry.network.message

import java.nio.ByteBuffer
import encry.network.PeerConnectionHandler._
import scorex.crypto.hash.Blake2b256
import scala.language.existentials
import scala.util.Try

case class MessageHandler(specs: Seq[MessageSpec[_]]) {

  import Message._

  private val specsMap = Map(specs.map(s => s.messageCode -> s): _*)
    .ensuring(m => m.size == specs.size, "Duplicate message codes")

  //MAGIC ++ Array(spec.messageCode) ++ Ints.toByteArray(dataLength) ++ dataWithChecksum
  def parseBytes(bytes: ByteBuffer, sourceOpt: Option[ConnectedPeer]): Try[Message[_]] = Try {
    val magic: Array[MessageCode] = new Array[Byte](MagicLength)
    bytes.get(magic)
    require(magic.sameElements(Message.MAGIC), "Wrong magic bytes" + magic.mkString)
    val msgCode: MessageCode = bytes.get
    val length: Int = bytes.getInt
    require(length >= 0, "Data length is negative!")
    val msgData: Array[Byte] = if (length > 0) {
      val data: Array[MessageCode] = new Array[Byte](length)
      val checksum: Array[MessageCode] = new Array[Byte](Message.ChecksumLength)
      bytes.get(checksum)
      bytes.get(data)
      val digest: Array[MessageCode] = Blake2b256.hash(data).take(Message.ChecksumLength)
      if (!checksum.sameElements(digest)) throw new Exception(s"Invalid data checksum length = $length")
      data
    } else Array()
    val spec: MessageSpec[_] = specsMap.get(msgCode) match {
      case Some(h) => h
      case None => throw new Exception(s"No message handler found for $msgCode")
    }
    Message(spec, Left(msgData), sourceOpt)
  }
}
