package encry.network

import java.net.{InetAddress, InetSocketAddress}
import com.google.common.primitives.{Ints, Longs}
import org.encryfoundation.common.serialization.{BytesSerializable, Serializer}
import scala.util.Try

case class Handshake(protocolVersion: Version,
                     nodeName: String,
                     declaredAddress: Option[InetSocketAddress],
                     time: Long) extends BytesSerializable {

  require(Option(protocolVersion).isDefined)

  override type M = Handshake

  override def serializer: Serializer[Handshake] = HandshakeSerializer
}

object HandshakeSerializer extends Serializer[Handshake] {

  override def toBytes(obj: Handshake): Array[Byte] = {
    val anb: Array[Byte] = "word".getBytes

    val fab: Array[Byte] = obj.declaredAddress.map { isa =>
      isa.getAddress.getAddress ++ Ints.toByteArray(isa.getPort)
    }.getOrElse(Array[Byte]())

    val nodeNameBytes: Array[Byte] = obj.nodeName.getBytes

    Array(anb.size.toByte) ++ anb ++
      obj.protocolVersion.bytes ++
      Array(nodeNameBytes.size.toByte) ++ nodeNameBytes ++
      Ints.toByteArray(fab.length) ++ fab ++
      Longs.toByteArray(obj.time)

  }

  override def parseBytes(bytes: Array[Byte]): Try[Handshake] = Try {
    var position = 0
    val appNameSize = bytes.head
    require(appNameSize > 0)

    position += 1

    val an: String = new String(bytes.slice(position, position + appNameSize))
    position += appNameSize

    val av: Version = ApplicationVersionSerializer.parseBytes(
      bytes.slice(position, position + ApplicationVersionSerializer.SerializedVersionLength)).get
    position += ApplicationVersionSerializer.SerializedVersionLength

    val nodeNameSize: Byte = bytes.slice(position, position + 1).head
    position += 1

    val nodeName: String = new String(bytes.slice(position, position + nodeNameSize))
    position += nodeNameSize

    val fas: Int = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4

    val isaOpt: Option[InetSocketAddress] = if (fas > 0) {
      val fa: Array[Byte] = bytes.slice(position, position + fas - 4)
      position += fas - 4

      val port: Int = Ints.fromByteArray(bytes.slice(position, position + 4))
      position += 4

      Some(new InetSocketAddress(InetAddress.getByAddress(fa), port))
    } else None

    val time: Long = Longs.fromByteArray(bytes.slice(position, position + 8))

    Handshake(av, nodeName, isaOpt, time)
  }
}