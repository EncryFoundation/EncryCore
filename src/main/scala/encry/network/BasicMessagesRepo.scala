package encry.network

import java.net.InetSocketAddress

import NetworkMessagesProto.GeneralizedNetworkProtoMessage
import NetworkMessagesProto.GeneralizedNetworkProtoMessage.InnerMessage
import NetworkMessagesProto.GeneralizedNetworkProtoMessage.InnerMessage.{GetPeersProtoMessage, HandshakeProtoMessage, InvProtoMessage, ModifiersProtoMessage, PeersProtoMessage, RequestModifiersProtoMessage, SyncInfoProtoMessage}
import NetworkMessagesProto.GeneralizedNetworkProtoMessage.ModifiersProtoMessage.MapFieldEntry
import NetworkMessagesProto.GeneralizedNetworkProtoMessage.{GetPeersProtoMessage => GetPeersProto, HandshakeProtoMessage => hPM, InvProtoMessage => InvPM, ModifiersProtoMessage => ModifiersPM, PeersProtoMessage => PeersPM, RequestModifiersProtoMessage => rModsPM, SyncInfoProtoMessage => sIPM}
import SyntaxMessageProto.InetSocketAddressProtoMessage
import com.google.protobuf.{ByteString => GoogleByteString}
import akka.util.{ByteString => AkkaByteString}
import com.typesafe.scalalogging.StrictLogging
import encry.network.BasicMessagesRepo.BasicMsgDataTypes.{InvData, ModifiersData}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.view.history.EncrySyncInfo
import scorex.crypto.hash.Blake2b256

import scala.util.Try

object BasicMessagesRepo extends StrictLogging {

  object BasicMsgDataTypes {
    type InvData = (ModifierTypeId, Seq[ModifierId])
    type ModifiersData = (ModifierTypeId, Map[ModifierId, Array[Byte]])
  }

  sealed trait NetworkMessage {

    val messageName: String

    def checkSumBytes(innerMessage: InnerMessage): Array[Byte]

    def toInnerMessage: InnerMessage
  }

  sealed trait ProtoNetworkMessagesSerializer[T] {

    def toProto(message: T): InnerMessage

    def fromProto(message: InnerMessage): Option[T]
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

  object MessageOptions {

    val MAGIC: GoogleByteString = GoogleByteString.copyFrom(Array[Byte](0x12: Byte, 0x34: Byte, 0x56: Byte, 0x78: Byte))

    val ChecksumLength: Int = 4

    def calculateCheckSum(bytes: Array[Byte]): GoogleByteString =
      GoogleByteString.copyFrom(Blake2b256.hash(bytes).take(ChecksumLength))
  }

  /**
    *
    * @param spec
    * @param source
    */

  case class MessageFromNetwork(spec: NetworkMessage, source: Option[ConnectedPeer])

  /**
    *
    */

  object GeneralizedNetworkMessage {

    def toProto(message: NetworkMessage): GeneralizedNetworkProtoMessage = {
      val innerMessage: InnerMessage = message.toInnerMessage
      val checkSumBytes: Array[Byte] = message.checkSumBytes(innerMessage)
      //TODO do we need it?
      //require(checkSumBytes.length > 0, "Empty checksum bytes!")
      val calculatedCheckSum: GoogleByteString = MessageOptions.calculateCheckSum(checkSumBytes)
      GeneralizedNetworkProtoMessage()
        .withMagic(MessageOptions.MAGIC)
        .withChecksum(calculatedCheckSum)
        .withInnerMessage(innerMessage)
    }

    def fromProto(message: AkkaByteString): Try[NetworkMessage] = Try {
      logger.info(s"GeneralizedNetworkMessage fromProto start")
      val netMessage: GeneralizedNetworkProtoMessage =
        GeneralizedNetworkProtoMessage.parseFrom(message.toArray)
      logger.info(s"GeneralizedNetworkMessage fromProto finished")
      require(netMessage.magic.toByteArray.sameElements(MessageOptions.MAGIC.toByteArray),
        s"Wrong MAGIC! Got ${netMessage.magic.toByteArray.mkString(",")}")
      netMessage.innerMessage match {
        case InnerMessage.SyncInfoProtoMessage(_) =>
          checkMessageValidity(SyncInfoNetworkMessageSerializer.fromProto, netMessage.innerMessage, netMessage.checksum)
        case InnerMessage.InvProtoMessage(_) =>
          checkMessageValidity(InvNetworkMessageSerializer.fromProto, netMessage.innerMessage, netMessage.checksum)
        case InnerMessage.RequestModifiersProtoMessage(_) =>
          checkMessageValidity(RequestModifiersSerializer.fromProto, netMessage.innerMessage, netMessage.checksum)
        case InnerMessage.ModifiersProtoMessage(_) =>
          checkMessageValidity(ModifiersNetworkMessageSerializer.fromProto, netMessage.innerMessage, netMessage.checksum)
        case InnerMessage.GetPeersProtoMessage(_) =>
          checkMessageValidity(GetPeersNetworkMessage.fromProto, netMessage.innerMessage, netMessage.checksum)
        case InnerMessage.PeersProtoMessage(_) =>
          checkMessageValidity(PeersNetworkMessageSerializer.fromProto, netMessage.innerMessage, netMessage.checksum)
        case InnerMessage.HandshakeProtoMessage(_) =>
          checkMessageValidity(HandshakeSerializer.fromProto, netMessage.innerMessage, netMessage.checksum)
        case InnerMessage.Empty => throw new RuntimeException("Empty inner message!")
        case _ => throw new RuntimeException("Can't find serializer for received message!")
      }
    }.flatten

    def checkMessageValidity(serializer: InnerMessage => Option[NetworkMessage],
                             innerMessage: InnerMessage,
                             requiredBytes: GoogleByteString): Try[NetworkMessage] = Try {
      val deserializedMessage: Option[NetworkMessage] = serializer(innerMessage)
      require(deserializedMessage.isDefined, "Nested message is invalid!")
      val networkMessage: NetworkMessage = deserializedMessage.get
      val checkSumBytes: Array[Byte] = networkMessage.checkSumBytes(innerMessage)
      val calculatedCheckSumBytes = MessageOptions.calculateCheckSum(checkSumBytes)
      require(calculatedCheckSumBytes.toByteArray.sameElements(requiredBytes.toByteArray),
        "Checksum of received message is invalid!")
      networkMessage
    }
  }

  /**
    *
    * @param esi
    */

  case class SyncInfoNetworkMessage(esi: EncrySyncInfo) extends NetworkMessage {

    override val messageName: String = "Sync"

    override def checkSumBytes(innerMessage: InnerMessage): Array[Byte] =
      innerMessage.syncInfoProtoMessage.map(_.toByteArray).getOrElse(Array.emptyByteArray)

    override def toInnerMessage: InnerMessage = SyncInfoNetworkMessageSerializer.toProto(this)
  }

  object SyncInfoNetworkMessageSerializer extends ProtoNetworkMessagesSerializer[SyncInfoNetworkMessage] {

    override def toProto(message: SyncInfoNetworkMessage): InnerMessage =
      SyncInfoProtoMessage(sIPM().withLastHeaderIds(message.esi.lastHeaderIds.map(GoogleByteString.copyFrom)))

    override def fromProto(message: InnerMessage): Option[SyncInfoNetworkMessage] = message.syncInfoProtoMessage match {
      case Some(value) =>
        Some(SyncInfoNetworkMessage(EncrySyncInfo(value.lastHeaderIds.map(modId => ModifierId @@ modId.toByteArray))))
      case None => Option.empty[SyncInfoNetworkMessage]
    }
  }

  /**
    *
    * @param data
    */

  case class InvNetworkMessage(data: InvData) extends NetworkMessage {

    override val messageName: String = "Inv"

    override def checkSumBytes(innerMessage: InnerMessage): Array[Byte] =
      innerMessage.invProtoMessage.map(_.toByteArray).getOrElse(Array.emptyByteArray)

    override def toInnerMessage: InnerMessage = InvNetworkMessageSerializer.toProto(this)
  }

  object InvNetworkMessageSerializer extends ProtoNetworkMessagesSerializer[InvNetworkMessage] {

    def toProto(message: InvNetworkMessage): InnerMessage = InvProtoMessage(InvPM()
      .withModifierTypeId(GoogleByteString.copyFrom(Array(message.data._1)))
      .withModifiers(message.data._2.map(elem => GoogleByteString.copyFrom(elem)))
    )

    def fromProto(message: InnerMessage): Option[InvNetworkMessage] = message.invProtoMessage match {
      case Some(value) => value.modifiers match {
        case mods: Seq[_] if mods.nonEmpty => Some(InvNetworkMessage(
          ModifierTypeId @@ value.modifierTypeId.toByteArray.head -> value.modifiers.map(x => ModifierId @@ x.toByteArray)))
        case _ => Option.empty[InvNetworkMessage]
      }
      case None => Option.empty[InvNetworkMessage]
    }
  }

  /**
    *
    * @param data - id's of requested modifiers. Includes modifierTypeId and Seq[ModifierId].
    *
    *             This message sends to the peer
    */

  case class RequestModifiersNetworkMessage(data: InvData) extends NetworkMessage {

    override val messageName: String = "RequestModifier"

    override def checkSumBytes(innerMessage: InnerMessage): Array[Byte] =
      innerMessage.requestModifiersProtoMessage.map(_.toByteArray).getOrElse(Array.emptyByteArray)

    override def toInnerMessage: InnerMessage = RequestModifiersSerializer.toProto(this)
  }

  object RequestModifiersSerializer extends ProtoNetworkMessagesSerializer[RequestModifiersNetworkMessage] {

    override def toProto(message: RequestModifiersNetworkMessage): InnerMessage =
      RequestModifiersProtoMessage(
        rModsPM()
          .withModifierTypeId(GoogleByteString.copyFrom(Array(message.data._1)))
          .withModifiers(message.data._2.map(elem => GoogleByteString.copyFrom(elem)))
      )

    override def fromProto(message: InnerMessage): Option[RequestModifiersNetworkMessage] =
      message.requestModifiersProtoMessage match {
        case Some(value) => value.modifiers match {
          case mods: Seq[_] if mods.nonEmpty => Some(RequestModifiersNetworkMessage(
            ModifierTypeId @@ value.modifierTypeId.toByteArray.head -> value.modifiers.map(x => ModifierId @@ x.toByteArray)))
          case _ => Option.empty[RequestModifiersNetworkMessage]
        }
        case None => Option.empty[RequestModifiersNetworkMessage]
      }
  }

  case class ModifiersNetworkMessage(data: ModifiersData) extends NetworkMessage {

    override val messageName: String = "Modifier"

    override def toInnerMessage: InnerMessage = ModifiersNetworkMessageSerializer.toProto(this)

    override def checkSumBytes(innerMessage: InnerMessage): Array[Byte] =
      innerMessage.modifiersProtoMessage.map(_.toByteArray).getOrElse(Array.emptyByteArray)
  }

  object ModifiersNetworkMessageSerializer extends ProtoNetworkMessagesSerializer[ModifiersNetworkMessage] {

    override def toProto(message: ModifiersNetworkMessage): InnerMessage = ModifiersProtoMessage(ModifiersPM()
      .withModifierTypeId(GoogleByteString.copyFrom(Array(message.data._1)))
      .withMap(message.data._2.map(element =>
        MapFieldEntry().withKey(GoogleByteString.copyFrom(element._1)).withValue(GoogleByteString.copyFrom(element._2))).toSeq))

    override def fromProto(message: InnerMessage): Option[ModifiersNetworkMessage] = message.modifiersProtoMessage match {
      case Some(value) => Some(ModifiersNetworkMessage(ModifierTypeId @@ value.modifierTypeId.toByteArray.head ->
        value.map.map(element => ModifierId @@ element.key.toByteArray -> element.value.toByteArray).toMap))
      case None => Option.empty[ModifiersNetworkMessage]
    }
  }

  /**
    * This network message sends to a random peer as a request for receiver's known peers.
    */

  case object GetPeersNetworkMessage extends NetworkMessage {

    override val messageName: String = "GetPeers message"

    override def toInnerMessage: InnerMessage = toProto

    def toProto: InnerMessage = GetPeersProtoMessage(GetPeersProto())

    def fromProto(message: InnerMessage): Option[GetPeersNetworkMessage.type] = message.getPeersProtoMessage match {
      case Some(_) => Some(GetPeersNetworkMessage)
      case None => Option.empty[GetPeersNetworkMessage.type]
    }

    override def checkSumBytes(innerMessage: InnerMessage): Array[Byte] =
      innerMessage.getPeersProtoMessage.map(_.toByteArray).getOrElse(Array.emptyByteArray)
  }

  /**
    *
    * @param peers - sequence of known by this peer another peers.
    *
    *              This network message sends directly to the sender of 'GetPeers' message.
    */

  case class PeersNetworkMessage(peers: Seq[InetSocketAddress]) extends NetworkMessage {

    override val messageName: String = "Peers message"

    override def toInnerMessage: InnerMessage = PeersNetworkMessageSerializer.toProto(this)

    override def checkSumBytes(innerMessage: InnerMessage): Array[Byte] =
      innerMessage.peersProtoMessage.map(_.toByteArray).getOrElse(Array.emptyByteArray)
  }

  object PeersNetworkMessageSerializer extends ProtoNetworkMessagesSerializer[PeersNetworkMessage] {

    override def toProto(message: PeersNetworkMessage): InnerMessage = PeersProtoMessage(
      PeersPM().withPeers(
        message.peers.map(element => InetSocketAddressProtoMessage().withHost(element.getHostName).withPort(element.getPort))
      ))

    override def fromProto(message: InnerMessage): Option[PeersNetworkMessage] = message.peersProtoMessage match {
      case Some(value) => value.peers match {
        case peers: Seq[_] if peers.nonEmpty =>
          Some(PeersNetworkMessage(value.peers.map(element => new InetSocketAddress(element.host, element.port))))
        case _ => Option.empty[PeersNetworkMessage]
      }
      case None => Option.empty[PeersNetworkMessage]
    }
  }

  /**
    *
    * @param protocolVersion - peer network communication protocol version
    * @param nodeName        - peer name
    * @param declaredAddress - peer address
    * @param time            - handshake creation time
    *
    *                        This network message are using for set up network connection between two peers.
    *                        First peer sends this message to the second one. Second peer processes this message
    *                        and send back response with it's own handshake. After both
    *                        peers received handshakes from each other, network connection raises.
    */

  case class Handshake(protocolVersion: Array[Byte],
                       nodeName: String,
                       declaredAddress: Option[InetSocketAddress],
                       time: Long) extends NetworkMessage {

    require(protocolVersion.length > 0, "Empty protocol version!")

    override val messageName: String = "Handshake"

    override def toInnerMessage: InnerMessage = HandshakeSerializer.toProto(this)

    override def checkSumBytes(innerMessage: InnerMessage): Array[Byte] =
      innerMessage.handshakeProtoMessage.map(_.toByteArray).getOrElse(Array.emptyByteArray)
  }

  object HandshakeSerializer extends ProtoNetworkMessagesSerializer[Handshake] {

    override def toProto(message: Handshake): InnerMessage = {
      val initialHandshakeProto: hPM = hPM()
        .withProtocolVersion(GoogleByteString.copyFrom(message.protocolVersion))
        .withNodeName(message.nodeName)
        .withTime(message.time)
      val updatedHandshakeProto: hPM = message.declaredAddress match {
        case Some(value) => initialHandshakeProto.withDeclaredAddress(InetSocketAddressProtoMessage()
          .withHost(value.getHostName)
          .withPort(value.getPort)
        )
        case None => initialHandshakeProto
      }
      HandshakeProtoMessage(updatedHandshakeProto)
    }

    override def fromProto(message: InnerMessage): Option[Handshake] = message.handshakeProtoMessage match {
      case Some(value) => value.nodeName match {
        case name: String if name.nonEmpty => Some(
          Handshake(
            value.protocolVersion.toByteArray,
            value.nodeName,
            value.declaredAddress.map(element => new InetSocketAddress(element.host, element.port)),
            value.time
          ))
        case _ => Option.empty[Handshake]
      }
      case None => Option.empty[Handshake]
    }
  }

}