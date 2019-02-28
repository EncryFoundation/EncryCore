package encry.network

import java.net.InetSocketAddress
import NetworkMessagesProto.GeneralNetworkProtoMessage.InnerMessage
import NetworkMessagesProto._
import SyntaxMessageProto.MapToProto.MapElementProto
import SyntaxMessageProto.{InetSocketAddressProtoMessage, MapToProto}
import com.google.protobuf.{ByteString => GoogleByteString}
import akka.util.{ByteString => AkkaByteString}
import encry.EncryApp.settings
import encry.network.BasicMessagesRepo.BasicMsgDataTypes.{InvData, ModifiersData}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.view.history.EncrySyncInfo
import scorex.crypto.hash.Blake2b256
import scala.util.Try

object BasicMessagesRepo {

  object BasicMsgDataTypes {
    type InvData = (ModifierTypeId, Seq[ModifierId])
    type ModifiersData = (ModifierTypeId, Map[ModifierId, Array[Byte]])
  }

  sealed trait NetworkMessage {

    val messageName: String

    def toBytes: Array[Byte]

    def toInnerMessage: InnerMessage
  }

  object NetworkMessagesIds {
    val GetPeers: Byte        = 1: Byte
    val Peers: Byte           = 2: Byte
    val RequestModifier: Byte = 22: Byte
    val Modifier: Byte        = 33: Byte
    val Inv: Byte             = 55: Byte
    val SyncInfo: Byte        = 65: Byte
    val HandShake: Byte       = 75: Byte
  }

  object MessageOptions {

    val MAGIC: Array[Byte] = Array[Byte](0x12: Byte, 0x34: Byte, 0x56: Byte, 0x78: Byte)

    val ChecksumLength: Int = 4

    def calculateCheckSum(bytes: Array[Byte]): GoogleByteString =
      GoogleByteString.copyFrom(Blake2b256.hash(bytes).take(ChecksumLength))
  }

  case class MessageFromNetwork(spec: NetworkMessage, source: Option[ConnectedPeer])

  object GeneralizedNetworkMessage {

    def toProto(messageToProto: NetworkMessage): GeneralNetworkProtoMessage =
      GeneralNetworkProtoMessage()
        .withMAGIC(GoogleByteString.copyFrom(MessageOptions.MAGIC))
        .withChecksum(MessageOptions.calculateCheckSum(messageToProto.toBytes))
        .withInnerMessage(messageToProto.toInnerMessage)

    def fromProto(message: AkkaByteString): Try[NetworkMessage] = Try {
      val generalNetworkMessageProto: GeneralNetworkProtoMessage = GeneralNetworkProtoMessage.parseFrom(message.toArray)
      require(generalNetworkMessageProto.mAGIC.toByteArray.sameElements(MessageOptions.MAGIC),
        s"Wrong MAGIC! Got ${generalNetworkMessageProto.mAGIC.toByteArray.mkString(",")}")
      val parsedMessage: Try[NetworkMessage] = generalNetworkMessageProto.innerMessage match {
        case message@InnerMessage.SyncInfoProtoMessage(_) => SyncInfoNetworkMessage.fromProto(message.value)
        case message@InnerMessage.InvSpecProtoMessage(_) => InvNetworkMessage.fromProto(message.value)
        case message@InnerMessage.RequestModifiersSpecMessageProto(_) => RequestModifiersNetworkMessage.fromProto(message.value)
        case message@InnerMessage.ModifiersSpecMessageProto(_) => ModifiersNetworkMessage.fromProto(message.value)
        case message@InnerMessage.GetPeersSpecMessageProto(_) => GetPeersNetworkMessage.fromProto(message.value)
        case message@InnerMessage.PeersSpecMessageProto(_) => PeersNetworkMessage.fromProto(message.value)
        case message@InnerMessage.HandshakeProtoMessage(_) => Handshake.fromProto(message.value)
      }
      require(parsedMessage.isSuccess, "Wrong type of nested message!")
      require(MessageOptions.calculateCheckSum(parsedMessage.get.toBytes).toByteArray
        .sameElements(generalNetworkMessageProto.checksum.toByteArray), s"Wrong hash of received message!")
      parsedMessage.get
    }
  }

  case class SyncInfoNetworkMessage(esi: EncrySyncInfo) extends NetworkMessage {

    override val messageName: String = "Sync"

    override def toBytes: Array[Byte] = SyncInfoNetworkMessage.toProto(this).toByteArray

    override def toInnerMessage: InnerMessage =
      InnerMessage.SyncInfoProtoMessage(SyncInfoNetworkMessage.toProto(this))
  }

  object SyncInfoNetworkMessage {

    def toProto(syncInfo: SyncInfoNetworkMessage): SyncInfoProtoMessage = SyncInfoProtoMessage()
      .withLastHeaderIds(syncInfo.esi.lastHeaderIds.map(elem => GoogleByteString.copyFrom(elem)))

    def fromProto(message: SyncInfoProtoMessage): Try[SyncInfoNetworkMessage] = Try {
      SyncInfoNetworkMessage(EncrySyncInfo(message.lastHeaderIds.map(x => ModifierId @@ x.toByteArray)))
    }
  }

  case class InvNetworkMessage(maxInvObjects: Int, data: InvData) extends NetworkMessage {

    override val messageName: String = "Inv"

    override def toBytes: Array[Byte] = InvNetworkMessage.toProto(this).toByteArray

    override def toInnerMessage: InnerMessage = InnerMessage.InvSpecProtoMessage(InvNetworkMessage.toProto(this))

    require(data._2.nonEmpty, "Empty inv message!")
    require(data._2.length < maxInvObjects, s"Inv message ${data._2.length} length bigger than max length $maxInvObjects!")
  }

  object InvNetworkMessage {

    def toProto(inv: InvNetworkMessage): InvSpecProtoMessage = InvSpecProtoMessage()
      .withModifierTypeId(GoogleByteString.copyFrom(Array(inv.data._1)))
      .withModifiers(inv.data._2.map(elem => GoogleByteString.copyFrom(elem)))

    def fromProto(message: InvSpecProtoMessage): Try[InvNetworkMessage] = Try(
      InvNetworkMessage(
        settings.network.maxInvObjects,
        ModifierTypeId @@ message.modifierTypeId.toByteArray.head -> message.modifiers.map(x => ModifierId @@ x.toByteArray)
      )
    )
  }

  case class RequestModifiersNetworkMessage(maxInvObjects: Int, data: InvData) extends NetworkMessage {

    override val messageName: String = "RequestModifier"

    override def toBytes: Array[Byte] = RequestModifiersNetworkMessage.toProto(this).toByteArray

    override def toInnerMessage: InnerMessage =
      InnerMessage.RequestModifiersSpecMessageProto(RequestModifiersNetworkMessage.toProto(this))

    require(data._2.nonEmpty, "Empty modifiers message!")
    require(data._2.length < maxInvObjects, s"Modifiers message ${data._2.length} length bigger than max length $maxInvObjects!")
  }

  object RequestModifiersNetworkMessage {

    def toProto(rm: RequestModifiersNetworkMessage): RequestModifiersSpecMessageProto =
      RequestModifiersSpecMessageProto()
        .withModifierTypeId(GoogleByteString.copyFrom(Array(rm.data._1)))
        .withModifiers(rm.data._2.map(x => GoogleByteString.copyFrom(x)))

    def fromProto(message: RequestModifiersSpecMessageProto): Try[RequestModifiersNetworkMessage] = Try(
      RequestModifiersNetworkMessage(
        settings.network.maxInvObjects,
        ModifierTypeId @@ message.modifierTypeId.toByteArray.head -> message.modifiers.map(x => ModifierId @@ x.toByteArray)
      )
    )
  }

  case class ModifiersNetworkMessage(data: ModifiersData) extends NetworkMessage {

    override val messageName: String = "Modifier"

    override def toBytes: Array[Byte] = ModifiersNetworkMessage.toProto(this).toByteArray

    override def toInnerMessage: InnerMessage =
      InnerMessage.ModifiersSpecMessageProto(ModifiersNetworkMessage.toProto(this))
  }

  object ModifiersNetworkMessage {

    def toProto(modN: ModifiersNetworkMessage): ModifiersSpecMessageProto = ModifiersSpecMessageProto()
      .withModifierTypeId(GoogleByteString.copyFrom(Array(modN.data._1)))
      .withMap(MapToProto().withKeyValue(modN.data._2.map(x =>
        MapElementProto().withKey(GoogleByteString.copyFrom(x._1)).withValue(GoogleByteString.copyFrom(x._2))).toSeq))

    def fromProto(message: ModifiersSpecMessageProto): Try[ModifiersNetworkMessage] = Try {
      ModifiersNetworkMessage(
        ModifierTypeId @@ message.modifierTypeId.toByteArray.head -> message.map.map(x => x.keyValue.map(k =>
          ModifierId @@ k.key.toByteArray -> k.value.toByteArray
        )).get.toMap)
    }
  }

  case object GetPeersNetworkMessage extends NetworkMessage {

    override val messageName: String = "GetPeers message"

    override def toBytes: Array[Byte] = GetPeersNetworkMessage.toProto.toByteArray

    override def toInnerMessage: InnerMessage = InnerMessage.GetPeersSpecMessageProto(GetPeersNetworkMessage.toProto)

    def toProto: GetPeersSpecMessageProto = GetPeersSpecMessageProto.defaultInstance

    def fromProto(message: GetPeersSpecMessageProto): Try[GetPeersNetworkMessage.type] = Try(GetPeersNetworkMessage)
  }

  case class PeersNetworkMessage(peers: Seq[InetSocketAddress]) extends NetworkMessage {

    override val messageName: String = "Peers message"

    override def toBytes: Array[Byte] = PeersNetworkMessage.toProto(this).toByteArray

    override def toInnerMessage: InnerMessage = InnerMessage.PeersSpecMessageProto(PeersNetworkMessage.toProto(this))
  }

  object PeersNetworkMessage {

    def toProto(peersN: PeersNetworkMessage): PeersSpecMessageProto = PeersSpecMessageProto()
      .withPeers(peersN.peers.map(x => InetSocketAddressProtoMessage().withHost(x.getHostName).withPort(x.getPort)))

    def fromProto(message: PeersSpecMessageProto): Try[PeersNetworkMessage] = Try {
      PeersNetworkMessage(message.peers.map(x => new InetSocketAddress(x.host, x.port)))
    }
  }

  case class Handshake(protocolVersion: Array[Byte],
                       nodeName: String,
                       declaredAddress: Option[InetSocketAddress],
                       time: Long) extends NetworkMessage {

    override val messageName: String = "Handshake"

    override def toBytes: Array[Byte] = Handshake.toProto(this).toByteArray

    override def toInnerMessage: InnerMessage = InnerMessage.HandshakeProtoMessage(Handshake.toProto(this))
  }

  object Handshake {

    def toProto(handshake: Handshake): HandshakeProtoMessage = HandshakeProtoMessage()
      .withProtocolVersion(GoogleByteString.copyFrom(handshake.protocolVersion))
      .withNodeName(handshake.nodeName)
      .withDeclaredAddress(handshake.declaredAddress match {
        case Some(value) => InetSocketAddressProtoMessage()
          .withHost(value.getHostName)
          .withPort(value.getPort)
        case None => InetSocketAddressProtoMessage.defaultInstance
      })
      .withTime(handshake.time)

    def fromProto(message: HandshakeProtoMessage): Try[Handshake] = Try {
      Handshake(
        message.protocolVersion.toByteArray,
        message.nodeName,
        message.declaredAddress.map(add => new InetSocketAddress(add.host, add.port)),
        message.time
      )
    }
  }
}