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

object BasicMsgDataTypes {
  type InvData = (ModifierTypeId, Seq[ModifierId])
  type ModifiersData = (ModifierTypeId, Map[ModifierId, Array[Byte]])
}

sealed trait NetworkMessage {
  val messageCode: Byte
  val messageName: String
}

case class SyncInfoNetworkMessage(si: EncrySyncInfo, peer: Option[ConnectedPeer]) extends NetworkMessage {

  override val messageCode: Byte = 65: Byte
  override val messageName: String = "Sync"

  def toProto: SyncInfoProtoMessage = SyncInfoProtoMessage()
    .withLastHeaderIds(si.lastHeaderIds.map(elem => ByteString.copyFrom(elem)))

  def fromProto(message: SyncInfoProtoMessage): SyncInfoNetworkMessage =
    SyncInfoNetworkMessage(EncrySyncInfo(message.lastHeaderIds.map(x => ModifierId @@ x.toByteArray)), None)
}

case class InvNetworkMessage(maxInvObjects: Int, data: InvData, peer: Option[ConnectedPeer]) extends NetworkMessage {

  override val messageCode: Byte = 55: Byte
  override val messageName: String = "Inv"

  //todo add req for MAX OBJ

  def toProto: InvSpecProtoMessage = InvSpecProtoMessage()
    .withModifierTypeId(ByteString.copyFrom(Array(data._1)))
    .withModifiers(data._2.map(elem => ByteString.copyFrom(elem)))

  def fromProto(message: InvSpecProtoMessage): InvNetworkMessage = InvNetworkMessage(
    settings.network.maxInvObjects,
    ModifierTypeId @@ message.modifierTypeId.toByteArray.head -> message.modifiers.map(x => ModifierId @@ x.toByteArray),
    None
  )
}

case class RequestModifiersNetworkMessage(maxInvObjects: Int, data: InvData, peer: Option[ConnectedPeer])
  extends NetworkMessage {

  //todo add req for MAX OBJ

  override val messageCode: Byte = 22: Byte
  override val messageName: String = "RequestModifier"

  def toProto: RequestModifiersSpecMessageProto = RequestModifiersSpecMessageProto()
    .withModifierTypeId(ByteString.copyFrom(Array(data._1)))
    .withModifiers(data._2.map(x => ByteString.copyFrom(x)))

  def fromProto(message: RequestModifiersSpecMessageProto): RequestModifiersNetworkMessage =
    RequestModifiersNetworkMessage(
      settings.network.maxInvObjects,
      ModifierTypeId @@ message.modifierTypeId.toByteArray.head -> message.modifiers.map(x => ModifierId @@ x.toByteArray),
      None
    )
}

case class ModifiersNetworkMessage(data: ModifiersData) extends NetworkMessage {

  override val messageCode: Byte = 33: Byte
  override val messageName: String = "Modifier"

  def toProto: ModifiersSpecMessageProto = ModifiersSpecMessageProto()
    .withModifierTypeId(ByteString.copyFrom(Array(data._1)))
    .withMap(MapToProto().withKeyValue(data._2.map(x =>
      MapElementProto().withKey(ByteString.copyFrom(x._1)).withValue(ByteString.copyFrom(x._2))).toSeq))

  def fromProto(message: ModifiersSpecMessageProto): ModifiersNetworkMessage = ModifiersNetworkMessage(
    ModifierTypeId @@ message.modifierTypeId.toByteArray.head -> message.map.map(x => x.keyValue.map(k =>
      ModifierId @@ k.key.toByteArray -> k.value.toByteArray
    )).get.toMap)
}

case class GetPeersNetworkMessage() extends NetworkMessage {

  override val messageCode: Byte = 1: Byte
  override val messageName: String = "GetPeers message"

  def toProto: GetPeersSpecMessageProto = GetPeersSpecMessageProto.defaultInstance

  def fromProto(message: GetPeersSpecMessageProto): GetPeersNetworkMessage = GetPeersNetworkMessage()
}

case class PeersNetworkMessage(peers: Seq[InetSocketAddress]) extends NetworkMessage {
  override val messageCode: Byte = 2: Byte
  override val messageName: String = "Peers message"

  def toProto: PeersSpecMessageProto = PeersSpecMessageProto()
    .withPeers(peers.map(x => InetSocketAddressProtoMessage().withHost(x.getHostName).withPort(x.getPort)))

  def fromProto(message: PeersSpecMessageProto): PeersNetworkMessage = PeersNetworkMessage(
    message.peers.map(x => new InetSocketAddress(x.host, x.port))
  )
}