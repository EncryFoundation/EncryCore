package encry.network.message

import java.net.InetSocketAddress

import NetworkMessagesProto._
import SyntaxMessageProto.{InetSocketAddressProtoMessage, MapToProto}
import SyntaxMessageProto.MapToProto.MapElementProto
import com.google.protobuf.ByteString
import encry.network.message.BasicMsgDataTypes.{InvData, _}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.view.history.EncrySyncInfo
import encry.EncryApp.settings
import encry.network.PeerConnectionHandler.ConnectedPeer
import akka.util.{ByteString => AByteString}

import scala.util.Try

object BasicMsgDataTypes {
  type InvData = (ModifierTypeId, Seq[ModifierId])
  type ModifiersData = (ModifierTypeId, Map[ModifierId, Array[Byte]])
}
///TODO ADD MAGIC TO ALL MASSAGES
///TODO ADD CHECKSUM TO ALL MESSAGES
///TODO REBUILD NETWORK MESSAGES IN PROTO CLASS

sealed trait NetworkMessage {
  val messageName: String
}

object NetworkMessagesIds {
  val GetPeers: Byte = 1: Byte
  val Peers: Byte = 2: Byte
  val RequestModifier: Byte = 22: Byte
  val Modifier: Byte = 33: Byte
  val Inv: Byte = 55: Byte
  val SyncInfo: Byte = 65: Byte
  val HandShake: Byte = 75: Byte
}

case class Message(spec: NetworkMessage, source: Option[ConnectedPeer])

case class SyncInfoNetworkMessage(si: EncrySyncInfo) extends NetworkMessage {

  override val messageName: String = "Sync"
}

object SyncInfoNetworkMessage {

  def toProto(sInfo: SyncInfoNetworkMessage): AByteString = AByteString(SyncInfoProtoMessage()
    .withLastHeaderIds(sInfo.si.lastHeaderIds.map(elem => ByteString.copyFrom(elem))).toByteArray)

  def fromProto(message: AByteString): Try[SyncInfoNetworkMessage] = Try {
    val syncInfoProtoMessage: SyncInfoProtoMessage = SyncInfoProtoMessage.parseFrom(message.toArray)
    SyncInfoNetworkMessage(EncrySyncInfo(syncInfoProtoMessage.lastHeaderIds.map(x => ModifierId @@ x.toByteArray)))
  }
}

case class InvNetworkMessage(maxInvObjects: Int, data: InvData) extends NetworkMessage {

  override val messageName: String = "Inv"

  require(data._2.nonEmpty, "Empty inv message!")
  require(data._2.length < maxInvObjects, s"Inv message ${data._2.length} length bigger than max length $maxInvObjects!")
}

object InvNetworkMessage {

  def toProto(inv: InvNetworkMessage): AByteString = AByteString(InvSpecProtoMessage()
    .withModifierTypeId(ByteString.copyFrom(Array(inv.data._1)))
    .withModifiers(inv.data._2.map(elem => ByteString.copyFrom(elem))).toByteArray)

  def fromProto(message: AByteString): Try[InvNetworkMessage] = Try {
    val invNetworkProtoMessage: InvSpecProtoMessage = InvSpecProtoMessage.parseFrom(message.toArray)
    InvNetworkMessage(
      settings.network.maxInvObjects,
      ModifierTypeId @@ invNetworkProtoMessage.modifierTypeId.toByteArray.head ->
        invNetworkProtoMessage.modifiers.map(x => ModifierId @@ x.toByteArray)
    )
  }
}

case class RequestModifiersNetworkMessage(maxInvObjects: Int, data: InvData)
  extends NetworkMessage {

  override val messageName: String = "RequestModifier"

  require(data._2.nonEmpty, "Empty modifiers message!")
  require(data._2.length < maxInvObjects, s"Modifiers message ${data._2.length} length bigger than max length $maxInvObjects!")
}

object RequestModifiersNetworkMessage {

  def toProto(rm: RequestModifiersNetworkMessage): AByteString = AByteString(RequestModifiersSpecMessageProto()
    .withModifierTypeId(ByteString.copyFrom(Array(rm.data._1)))
    .withModifiers(rm.data._2.map(x => ByteString.copyFrom(x))).toByteArray)

  def fromProto(message: AByteString): Try[RequestModifiersNetworkMessage] = Try {
    val requestModifiersSpecMessageProto: RequestModifiersSpecMessageProto =
      RequestModifiersSpecMessageProto.parseFrom(message.toArray)
    RequestModifiersNetworkMessage(
      settings.network.maxInvObjects,
      ModifierTypeId @@ requestModifiersSpecMessageProto.modifierTypeId.toByteArray.head ->
        requestModifiersSpecMessageProto.modifiers.map(x => ModifierId @@ x.toByteArray)
    )
  }
}

case class ModifiersNetworkMessage(data: ModifiersData) extends NetworkMessage {

  override val messageName: String = "Modifier"
}

object ModifiersNetworkMessage {

  def toProto(modN: ModifiersNetworkMessage): AByteString = AByteString(ModifiersSpecMessageProto()
    .withModifierTypeId(ByteString.copyFrom(Array(modN.data._1)))
    .withMap(MapToProto().withKeyValue(modN.data._2.map(x =>
      MapElementProto().withKey(ByteString.copyFrom(x._1)).withValue(ByteString.copyFrom(x._2))).toSeq)).toByteArray)

  def fromProto(message: AByteString): Try[ModifiersNetworkMessage] = Try {
    val modifiersSpecMessageProto: ModifiersSpecMessageProto = ModifiersSpecMessageProto.parseFrom(message.toArray)
    ModifiersNetworkMessage(
      ModifierTypeId @@ modifiersSpecMessageProto.modifierTypeId.toByteArray.head ->
        modifiersSpecMessageProto.map.map(x => x.keyValue.map(k =>
          ModifierId @@ k.key.toByteArray -> k.value.toByteArray
        )).get.toMap)
  }
}

case class GetPeersNetworkMessage() extends NetworkMessage {

  override val messageName: String = "GetPeers message"
}

object GetPeersNetworkMessage {

  def toProto: AByteString = AByteString(GetPeersSpecMessageProto.defaultInstance.toByteArray)

  def fromProto(message: AByteString): Try[GetPeersNetworkMessage] = Try {
    GetPeersSpecMessageProto.parseFrom(message.toArray)
    GetPeersNetworkMessage()
  }
}

case class PeersNetworkMessage(peers: Seq[InetSocketAddress]) extends NetworkMessage {

  override val messageName: String = "Peers message"
}

object PeersNetworkMessage {

  def toProto(peersN: PeersNetworkMessage): AByteString = AByteString(PeersSpecMessageProto()
    .withPeers(peersN.peers.map(x =>
      InetSocketAddressProtoMessage().withHost(x.getHostName).withPort(x.getPort))).toByteArray)

  def fromProto(message: AByteString): Try[PeersNetworkMessage] = Try {
    val peersSpecMessageProto: PeersSpecMessageProto = PeersSpecMessageProto.parseFrom(message.toArray)
    PeersNetworkMessage(peersSpecMessageProto.peers.map(x => new InetSocketAddress(x.host, x.port)))
  }
}

case class Handshake(protocolVersion: Array[Byte],
                     nodeName: String,
                     declaredAddress: Option[InetSocketAddress],
                     time: Long) extends NetworkMessage {

  override val messageName: String = "Handshake"
}

object Handshake {

  def toProto(handshake: Handshake): AByteString = AByteString(HandshakeProtoMessage()
    .withProtocolVersion(ByteString.copyFrom(handshake.protocolVersion))
    .withNodeName(handshake.nodeName)
    .withDeclaredAddress(handshake.declaredAddress match {
      case Some(value) => InetSocketAddressProtoMessage()
        .withHost(value.getHostName)
        .withPort(value.getPort)
      case None => InetSocketAddressProtoMessage.defaultInstance
    })
    .withTime(handshake.time).toByteArray)

  def fromProto(message: AByteString): Try[Handshake] = Try {
    val handshakeProtoMessage: HandshakeProtoMessage = HandshakeProtoMessage.parseFrom(message.toArray)
    Handshake(
      handshakeProtoMessage.protocolVersion.toByteArray,
      handshakeProtoMessage.nodeName,
      handshakeProtoMessage.declaredAddress.map(add => new InetSocketAddress(add.host, add.port)),
      handshakeProtoMessage.time
    )
  }
}