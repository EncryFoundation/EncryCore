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
    val magic = new Array[Byte](MagicLength)
    bytes.get(magic)

    require(magic.sameElements(Message.MAGIC), "Wrong magic bytes" + magic.mkString)

    val msgCode = bytes.get

    val length = bytes.getInt
    require(length >= 0, "Data length is negative!")

    val msgData: Array[Byte] = if (length > 0) {
      val data = new Array[Byte](length)
      //READ CHECKSUM
      val checksum = new Array[Byte](Message.ChecksumLength)
      bytes.get(checksum)

      //READ DATA
      bytes.get(data)

      //VALIDATE CHECKSUM
      val digest = Blake2b256.hash(data).take(Message.ChecksumLength)

      //CHECK IF CHECKSUM MATCHES
      if(!checksum.sameElements(digest)) throw new Error(s"Invalid data checksum length = $length")
      data
    }
    else Array()

    val spec = specsMap.get(msgCode) match {
      case Some(h) => h
      case None => throw new Error(s"No message handler found for $msgCode")
    }

    Message(spec, Left(msgData), sourceOpt)
  }
}
