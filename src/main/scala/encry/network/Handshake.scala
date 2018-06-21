package encry.network

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.primitives.{Ints, Longs}
import encry.modifiers.{BytesSerializable, Serializer}
import encry.network.ApplicationVersionSerializer

import scala.util.Try


case class Handshake(applicationName: String,
                     protocolVersion: Version,
                     nodeName: String,
                     declaredAddress: Option[InetSocketAddress],
                     time: Long) extends BytesSerializable {

  require(Option(applicationName).isDefined)
  require(Option(protocolVersion).isDefined)

  override type M = Handshake

  override def serializer: Serializer[Handshake] = HandshakeSerializer
}

object HandshakeSerializer extends Serializer[Handshake] {

  override def toBytes(obj: Handshake): Array[Byte] = {
    val anb = obj.applicationName.getBytes

    val fab = obj.declaredAddress.map { isa =>
      isa.getAddress.getAddress ++ Ints.toByteArray(isa.getPort)
    }.getOrElse(Array[Byte]())

    val nodeNameBytes = obj.nodeName.getBytes

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

    val an = new String(bytes.slice(position, position + appNameSize))
    position += appNameSize

    val av = ApplicationVersionSerializer.parseBytes(
      bytes.slice(position, position + ApplicationVersionSerializer.SerializedVersionLength)).get
    position += ApplicationVersionSerializer.SerializedVersionLength

    val nodeNameSize = bytes.slice(position, position + 1).head
    position += 1

    val nodeName = new String(bytes.slice(position, position + nodeNameSize))
    position += nodeNameSize

    val fas = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4

    val isaOpt = if (fas > 0) {
      val fa = bytes.slice(position, position + fas - 4)
      position += fas - 4

      val port = Ints.fromByteArray(bytes.slice(position, position + 4))
      position += 4

      Some(new InetSocketAddress(InetAddress.getByAddress(fa), port))
    } else None

    val time = Longs.fromByteArray(bytes.slice(position, position + 8))

    Handshake(an, av, nodeName, isaOpt, time)
  }
}