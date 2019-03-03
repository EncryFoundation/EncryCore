package encry.network.NetworkMessagesProtoTest

import java.net.InetSocketAddress
import NetworkMessagesProto.GeneralizedNetworkProtoMessage
import NetworkMessagesProto.GeneralizedNetworkProtoMessage.InnerMessage,
import akka.util.ByteString
import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header, Payload}
import encry.modifiers.mempool.Transaction
import encry.network.BasicMessagesRepo
import encry.network.BasicMessagesRepo._
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.utils.EncryGenerator
import encry.view.history.EncrySyncInfo
import org.scalatest.{Matchers, PropSpec}
import scala.util.Try

class BasicNetworkMessagesProtoTest extends PropSpec with Matchers with InstanceFactory with EncryGenerator {

  val settings: EncryAppSettings = EncryAppSettings.read
  val testedBlocks: Vector[Block] = (0 until 10).foldLeft(generateDummyHistory(settings), Vector.empty[Block]) {
    case ((prevHistory, blocks), _) =>
      val block: Block = generateNextBlock(prevHistory)
      (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
        blocks :+ block)
  }._2
  val testedTransaction: Seq[Transaction] = genValidPaymentTxs(10)

  val firstAddress = new InetSocketAddress("172.16.11.10", 9000)
  val secondAddress = new InetSocketAddress("172.16.11.11", 9001)
  val thirdAddress = new InetSocketAddress("172.16.11.12", 9002)
  val peers: Seq[InetSocketAddress] = Seq(firstAddress, secondAddress, thirdAddress)

  val invDataHeaders: (ModifierTypeId, Vector[ModifierId]) = Header.modifierTypeId -> testedBlocks.map(_.header.id)
  val invDataPayloads: (ModifierTypeId, Vector[ModifierId]) = Payload.modifierTypeId -> testedBlocks.map(_.payload.id)
  val invDataTransactions: (ModifierTypeId, Seq[ModifierId]) = Transaction.ModifierTypeId -> testedTransaction.map(_.id)

  val invDataHeadersDummy: (ModifierTypeId, Vector[ModifierId]) = Header.modifierTypeId -> Vector.empty[ModifierId]
  val invDataPayloadsDummy: (ModifierTypeId, Vector[ModifierId]) = Payload.modifierTypeId -> Vector.empty[ModifierId]
  val invDataTransactionsDummy: (ModifierTypeId, Seq[ModifierId]) = Transaction.ModifierTypeId -> Vector.empty[ModifierId]

  def protocolToBytes(protocol: String): Array[Byte] = protocol.split("\\.").map(elem => elem.toByte)

  property("SyncInfoMessage should be serialized correctly") {
    val syncInfo: EncrySyncInfo = EncrySyncInfo(testedBlocks.map(_.header.id))
    val syncInfoBeforeProto: SyncInfoNetworkMessage = SyncInfoNetworkMessage(syncInfo)
    val syncInfoToProto: GeneralizedNetworkProtoMessage.InnerMessage = syncInfoBeforeProto.toInnerMessage
    val syncInfoFromProto: Option[SyncInfoNetworkMessage] = SyncInfoNetworkMessageSerializer.fromProto(syncInfoToProto)

    val comparison: Set[Boolean] = syncInfoFromProto.get.esi.lastHeaderIds.map(id =>
      syncInfoBeforeProto.esi.lastHeaderIds.exists(element => id.sameElements(element))).toSet
    comparison.size shouldEqual 1
    comparison.head shouldEqual true

    val generalizedNetworkMessageToProto: GeneralizedNetworkProtoMessage = GeneralizedNetworkMessage.toProto(syncInfoBeforeProto)
    val generalizedNetworkMessageFromProto: Try[NetworkMessage] =
      GeneralizedNetworkMessage.fromProto(ByteString(generalizedNetworkMessageToProto.toByteArray))

    generalizedNetworkMessageFromProto.isSuccess shouldBe true

    val receivedMessage: SyncInfoNetworkMessage = generalizedNetworkMessageFromProto.get match {
      case si: SyncInfoNetworkMessage => si
    }
    val comparisonReceivedMessage: Set[Boolean] = receivedMessage.esi.lastHeaderIds.map(id =>
      syncInfoBeforeProto.esi.lastHeaderIds.exists(element => id.sameElements(element))).toSet
    comparisonReceivedMessage.size shouldEqual 1
    comparisonReceivedMessage.head shouldEqual true

    val syncInfoWithoutModifiers: EncrySyncInfo = EncrySyncInfo(Seq.empty[ModifierId])
    val syncInfoBeforeProtoWithoutModifiers: SyncInfoNetworkMessage = SyncInfoNetworkMessage(syncInfoWithoutModifiers)
    val syncInfoToProtoWithoutModifiers: GeneralizedNetworkProtoMessage.InnerMessage = syncInfoBeforeProtoWithoutModifiers.toInnerMessage
    val syncInfoFromProtoWithoutModifiers: Option[SyncInfoNetworkMessage] = SyncInfoNetworkMessageSerializer.fromProto(syncInfoToProtoWithoutModifiers)

    syncInfoFromProtoWithoutModifiers.get.esi.lastHeaderIds.length shouldBe 0

    val generalizedNetworkMessageToProtoSIWithoutModifiers: GeneralizedNetworkProtoMessage =
      GeneralizedNetworkMessage.toProto(syncInfoBeforeProtoWithoutModifiers)
    val generalizedNetworkMessageFromProtoSIWithoutModifiers: Try[NetworkMessage] =
      GeneralizedNetworkMessage.fromProto(ByteString(generalizedNetworkMessageToProtoSIWithoutModifiers.toByteArray))

    generalizedNetworkMessageFromProtoSIWithoutModifiers.isSuccess shouldBe true

    val receivedMessageWO: SyncInfoNetworkMessage = generalizedNetworkMessageFromProtoSIWithoutModifiers.get match {
      case si: SyncInfoNetworkMessage => si
    }

    receivedMessageWO.esi.lastHeaderIds.size shouldEqual 0
  }

  property("InvNetworkMessage should be serialized correctly") {
    /**
      *
      */
    val invNetworkMessageWithHeadersBeforeProto: InvNetworkMessage = InvNetworkMessage(invDataHeaders)
    val invNetworkMessageWithHeadersToProto: InnerMessage = invNetworkMessageWithHeadersBeforeProto.toInnerMessage
    val invNetworkMessageWithHeadersFromProto: Option[InvNetworkMessage] =
      InvNetworkMessageSerializer.fromProto(invNetworkMessageWithHeadersToProto)

    invNetworkMessageWithHeadersBeforeProto.data._1 == invNetworkMessageWithHeadersFromProto.get.data._1
    val comparisonHeaders: Set[Boolean] = invNetworkMessageWithHeadersBeforeProto.data._2.map(id =>
      invNetworkMessageWithHeadersFromProto.get.data._2.exists(element => id.sameElements(element))).toSet
    comparisonHeaders.size shouldEqual 1
    comparisonHeaders.head shouldEqual true

    /**
      *
      */
    val generalizedNetworkMessageToProtoHeaders: GeneralizedNetworkProtoMessage = GeneralizedNetworkMessage.toProto(invNetworkMessageWithHeadersBeforeProto)
    val generalizedNetworkMessageFromProtoHeaders: Try[NetworkMessage] =
      GeneralizedNetworkMessage.fromProto(ByteString(generalizedNetworkMessageToProtoHeaders.toByteArray))

    generalizedNetworkMessageFromProtoHeaders.isSuccess shouldBe true

    val receivedMessageH: InvNetworkMessage = generalizedNetworkMessageFromProtoHeaders.get match {
      case inv: InvNetworkMessage => inv
    }
    receivedMessageH.data._1 == invNetworkMessageWithHeadersBeforeProto.data._1
    val comparisonReceivedMessageH: Set[Boolean] = receivedMessageH.data._2.map(id =>
      invNetworkMessageWithHeadersBeforeProto.data._2.exists(element => id.sameElements(element))).toSet
    comparisonReceivedMessageH.size shouldEqual 1
    comparisonReceivedMessageH.head shouldEqual true

    /**
      *
      */
    val invNetworkMessageWithPayloadsBeforeProto: InvNetworkMessage = InvNetworkMessage(invDataPayloads)
    val invNetworkMessageWithPayloadsToProto: InnerMessage = invNetworkMessageWithPayloadsBeforeProto.toInnerMessage
    val invNetworkMessageWithPayloadsFromProto: Option[InvNetworkMessage] =
      InvNetworkMessageSerializer.fromProto(invNetworkMessageWithPayloadsToProto)

    invNetworkMessageWithPayloadsBeforeProto.data._1 == invNetworkMessageWithPayloadsFromProto.get.data._1
    val comparisonPayloads: Set[Boolean] = invNetworkMessageWithPayloadsBeforeProto.data._2.map(id =>
      invNetworkMessageWithPayloadsFromProto.get.data._2.exists(element => id.sameElements(element))).toSet
    comparisonPayloads.size shouldEqual 1
    comparisonPayloads.head shouldEqual true

    /**
      *
      */

    val generalizedNetworkMessageToProtoPayloads: GeneralizedNetworkProtoMessage = GeneralizedNetworkMessage.toProto(invNetworkMessageWithPayloadsBeforeProto)
    val generalizedNetworkMessageFromProtoPayloads: Try[NetworkMessage] =
      GeneralizedNetworkMessage.fromProto(ByteString(generalizedNetworkMessageToProtoPayloads.toByteArray))

    generalizedNetworkMessageFromProtoPayloads.isSuccess shouldBe true

    val receivedMessageP: InvNetworkMessage = generalizedNetworkMessageFromProtoPayloads.get match {
      case inv: InvNetworkMessage => inv
    }
    receivedMessageP.data._1 == invNetworkMessageWithPayloadsBeforeProto.data._1
    val comparisonReceivedMessageP: Set[Boolean] = receivedMessageP.data._2.map(id =>
      invNetworkMessageWithPayloadsBeforeProto.data._2.exists(element => id.sameElements(element))).toSet
    comparisonReceivedMessageP.size shouldEqual 1
    comparisonReceivedMessageP.head shouldEqual true

    /**
      *
      */
    val invNetworkMessageWithTransactionsBeforeProto: InvNetworkMessage = InvNetworkMessage(invDataTransactions)
    val invNetworkMessageWithTransactionsToProto: InnerMessage = invNetworkMessageWithTransactionsBeforeProto.toInnerMessage
    val invNetworkMessageWithTransactionsFromProto: Option[InvNetworkMessage] =
      InvNetworkMessageSerializer.fromProto(invNetworkMessageWithTransactionsToProto)

    invNetworkMessageWithTransactionsBeforeProto.data._1 == invNetworkMessageWithTransactionsFromProto.get.data._1
    val comparisonTransactions: Set[Boolean] = invNetworkMessageWithTransactionsBeforeProto.data._2.map(id =>
      invNetworkMessageWithTransactionsFromProto.get.data._2.exists(element => id.sameElements(element))).toSet
    comparisonTransactions.size shouldEqual 1
    comparisonTransactions.head shouldEqual true

    /**
      *
      */

    val generalizedNetworkMessageToProtoTx: GeneralizedNetworkProtoMessage = GeneralizedNetworkMessage.toProto(invNetworkMessageWithTransactionsBeforeProto)
    val generalizedNetworkMessageFromProtoTx: Try[NetworkMessage] =
      GeneralizedNetworkMessage.fromProto(ByteString(generalizedNetworkMessageToProtoTx.toByteArray))

    generalizedNetworkMessageFromProtoTx.isSuccess shouldBe true

    val receivedMessageT: InvNetworkMessage = generalizedNetworkMessageFromProtoTx.get match {
      case inv: InvNetworkMessage => inv
    }
    receivedMessageT.data._1 == invNetworkMessageWithTransactionsBeforeProto.data._1
    val comparisonReceivedMessageT: Set[Boolean] = receivedMessageT.data._2.map(id =>
      invNetworkMessageWithTransactionsBeforeProto.data._2.exists(element => id.sameElements(element))).toSet
    comparisonReceivedMessageT.size shouldEqual 1
    comparisonReceivedMessageT.head shouldEqual true
    /**
      *
      */
    val invNetworkMessageWithHeadersBeforeProtoDummy: InvNetworkMessage = InvNetworkMessage(invDataHeadersDummy)
    val invNetworkMessageWithHeadersToProtoDummy: InnerMessage = invNetworkMessageWithHeadersBeforeProtoDummy.toInnerMessage
    val invNetworkMessageWithHeadersFromProtoDummy: Option[InvNetworkMessage] =
      InvNetworkMessageSerializer.fromProto(invNetworkMessageWithHeadersToProtoDummy)

    invNetworkMessageWithHeadersFromProtoDummy shouldBe None

    /**
      *
      */
    val invNetworkMessageWithPayloadsBeforeProtoDummy: InvNetworkMessage = InvNetworkMessage(invDataPayloadsDummy)
    val invNetworkMessageWithPayloadsToProtoDummy: InnerMessage = invNetworkMessageWithPayloadsBeforeProtoDummy.toInnerMessage
    val invNetworkMessageWithPayloadsFromProtoDummy: Option[InvNetworkMessage] =
      InvNetworkMessageSerializer.fromProto(invNetworkMessageWithPayloadsToProtoDummy)

    invNetworkMessageWithPayloadsFromProtoDummy shouldBe None

    /**
      *
      */
    val invNetworkMessageWithTransactionsBeforeProtoDummy: InvNetworkMessage = InvNetworkMessage(invDataTransactionsDummy)
    val invNetworkMessageWithTransactionsToProtoDummy: InnerMessage = invNetworkMessageWithTransactionsBeforeProtoDummy.toInnerMessage
    val invNetworkMessageWithTransactionsFromProtoDummy: Option[InvNetworkMessage] =
      InvNetworkMessageSerializer.fromProto(invNetworkMessageWithTransactionsToProtoDummy)

    invNetworkMessageWithTransactionsFromProtoDummy shouldBe None
  }

  property("RequestModifiersNetworkMessage should be serialized correctly") {
    val requestModifiersWithHeadersBeforeProto: RequestModifiersNetworkMessage = RequestModifiersNetworkMessage(invDataHeaders)
    val requestModifiersWithHeadersToProto: InnerMessage = requestModifiersWithHeadersBeforeProto.toInnerMessage
    val requestModifiersWithHeadersFromProto: Option[RequestModifiersNetworkMessage] =
      RequestModifiersSerializer.fromProto(requestModifiersWithHeadersToProto)

    requestModifiersWithHeadersBeforeProto.data._1 == requestModifiersWithHeadersFromProto.get.data._1
    val comparisonHeaders: Set[Boolean] = requestModifiersWithHeadersBeforeProto.data._2.map(id =>
      requestModifiersWithHeadersFromProto.get.data._2.exists(element => id.sameElements(element))).toSet
    comparisonHeaders.size shouldEqual 1
    comparisonHeaders.head shouldEqual true

    val generalizedNetworkMessageToProtoHeaders: GeneralizedNetworkProtoMessage = GeneralizedNetworkMessage.toProto(requestModifiersWithHeadersBeforeProto)
    val generalizedNetworkMessageFromProtoHeaders: Try[NetworkMessage] =
      GeneralizedNetworkMessage.fromProto(ByteString(generalizedNetworkMessageToProtoHeaders.toByteArray))

    generalizedNetworkMessageFromProtoHeaders.isSuccess shouldBe true

    val receivedMessageH: RequestModifiersNetworkMessage = generalizedNetworkMessageFromProtoHeaders.get match {
      case req: RequestModifiersNetworkMessage => req
    }
    receivedMessageH.data._1 == requestModifiersWithHeadersBeforeProto.data._1
    val comparisonReceivedMessageH: Set[Boolean] = receivedMessageH.data._2.map(id =>
      requestModifiersWithHeadersBeforeProto.data._2.exists(element => id.sameElements(element))).toSet
    comparisonReceivedMessageH.size shouldEqual 1
    comparisonReceivedMessageH.head shouldEqual true

    val requestModifiersWithPayloadsBeforeProto: RequestModifiersNetworkMessage = RequestModifiersNetworkMessage(invDataPayloads)
    val requestModifiersWithPayloadsToProto: InnerMessage = requestModifiersWithPayloadsBeforeProto.toInnerMessage
    val requestModifiersWithPayloadsFromProto: Option[RequestModifiersNetworkMessage] =
      RequestModifiersSerializer.fromProto(requestModifiersWithPayloadsToProto)

    requestModifiersWithPayloadsBeforeProto.data._1 == requestModifiersWithPayloadsFromProto.get.data._1
    val comparisonPayloads: Set[Boolean] = requestModifiersWithPayloadsBeforeProto.data._2.map(id =>
      requestModifiersWithPayloadsFromProto.get.data._2.exists(element => id.sameElements(element))).toSet
    comparisonPayloads.size shouldEqual 1
    comparisonPayloads.head shouldEqual true

    val requestModifiersWithTransactionsBeforeProto: RequestModifiersNetworkMessage = RequestModifiersNetworkMessage(invDataTransactions)
    val requestModifiersWithTransactionsToProto: InnerMessage = requestModifiersWithTransactionsBeforeProto.toInnerMessage
    val requestModifiersWithTransactionsFromProto: Option[RequestModifiersNetworkMessage] =
      RequestModifiersSerializer.fromProto(requestModifiersWithTransactionsToProto)

    requestModifiersWithTransactionsBeforeProto.data._1 == requestModifiersWithTransactionsFromProto.get.data._1
    val comparisonTransactions: Set[Boolean] = requestModifiersWithTransactionsBeforeProto.data._2.map(id =>
      requestModifiersWithTransactionsFromProto.get.data._2.exists(element => id.sameElements(element))).toSet
    comparisonTransactions.size shouldEqual 1
    comparisonTransactions.head shouldEqual true

    val requestModifiersWithHeadersBeforeProtoDummy: RequestModifiersNetworkMessage = RequestModifiersNetworkMessage(invDataHeadersDummy)
    val requestModifiersWithHeadersToProtoDummy: InnerMessage = requestModifiersWithHeadersBeforeProtoDummy.toInnerMessage
    val requestModifiersWithHeadersFromProtoDummy: Option[RequestModifiersNetworkMessage] =
      RequestModifiersSerializer.fromProto(requestModifiersWithHeadersToProtoDummy)

    requestModifiersWithHeadersFromProtoDummy shouldBe None

    val generalizedNetworkMessageToProtoHeadersDummy: GeneralizedNetworkProtoMessage =
      GeneralizedNetworkMessage.toProto(requestModifiersWithHeadersBeforeProtoDummy)
    val generalizedNetworkMessageFromProtoHeadersDummy: Try[NetworkMessage] =
      GeneralizedNetworkMessage.fromProto(ByteString(generalizedNetworkMessageToProtoHeadersDummy.toByteArray))

    generalizedNetworkMessageFromProtoHeadersDummy.isFailure shouldBe true

    val requestModifiersWithPayloadsBeforeProtoDummy: RequestModifiersNetworkMessage = RequestModifiersNetworkMessage(invDataPayloadsDummy)
    val requestModifiersWithPayloadsToProtoDummy: InnerMessage = requestModifiersWithPayloadsBeforeProtoDummy.toInnerMessage
    val requestModifiersWithPayloadsFromProtoDummy: Option[RequestModifiersNetworkMessage] =
      RequestModifiersSerializer.fromProto(requestModifiersWithPayloadsToProtoDummy)

    requestModifiersWithPayloadsFromProtoDummy shouldBe None

    val requestModifiersWithTransactionsBeforeProtoDummy: RequestModifiersNetworkMessage = RequestModifiersNetworkMessage(invDataTransactionsDummy)
    val requestModifiersWithTransactionsToProtoDummy: InnerMessage = requestModifiersWithTransactionsBeforeProtoDummy.toInnerMessage
    val requestModifiersWithTransactionsFromProtoDummy: Option[RequestModifiersNetworkMessage] =
      RequestModifiersSerializer.fromProto(requestModifiersWithTransactionsToProtoDummy)

    requestModifiersWithTransactionsFromProtoDummy shouldBe None
  }

  property("ModifiersNetworkMessage should be serialized correctly") {
    val modifiersDataAsHeader: (ModifierTypeId, Map[ModifierId, Array[Byte]]) =
      Header.modifierTypeId -> testedBlocks.map(block => block.header.id -> block.header.toHeaderProto.toByteArray).toMap
    val modifiersDataAsPayloads: (ModifierTypeId, Map[ModifierId, Array[Byte]]) =
      Payload.modifierTypeId -> testedBlocks.map(block => block.payload.id -> block.payload.toProtoPayload.toByteArray).toMap
    val modifiersDataAsTransactions: (ModifierTypeId, Map[ModifierId, Array[Byte]]) =
      Transaction.ModifierTypeId -> testedTransaction.map(tx => tx.id -> tx.toTransactionProto.toByteArray).toMap

    val modifiersNetworkMessageHeaders: ModifiersNetworkMessage = ModifiersNetworkMessage(modifiersDataAsHeader)
    val modifiersNetworkMessagePayloads: ModifiersNetworkMessage = ModifiersNetworkMessage(modifiersDataAsPayloads)
    val modifiersNetworkMessageTransactions: ModifiersNetworkMessage = ModifiersNetworkMessage(modifiersDataAsTransactions)

    val modifiersNetworkMessageHeadersToProto = modifiersNetworkMessageHeaders.toInnerMessage
    val modifiersNetworkMessagePayloadsToProto = modifiersNetworkMessagePayloads.toInnerMessage
    val modifiersNetworkMessageTransactionsToProto = modifiersNetworkMessageTransactions.toInnerMessage

    val modifiersNetworkMessageHeadersFromProto = ModifiersNetworkMessageSerializer.fromProto(modifiersNetworkMessageHeadersToProto)
    val modifiersNetworkMessagePayloadsFromProto = ModifiersNetworkMessageSerializer.fromProto(modifiersNetworkMessagePayloadsToProto)
    val modifiersNetworkMessageTransactionsFromProto = ModifiersNetworkMessageSerializer.fromProto(modifiersNetworkMessageTransactionsToProto)

    val compr1: Set[Boolean] = modifiersNetworkMessageHeaders.data._2.keys.map(el => modifiersNetworkMessageHeadersFromProto.get.data._2.keys
      .exists(id => id.sameElements(el))).toSet
    compr1.size shouldBe 1
    compr1.head shouldBe true
    val compr2 = modifiersNetworkMessageHeaders.data._2.values.map(el => modifiersNetworkMessageHeadersFromProto.get.data._2.values
      .exists(id => id.sameElements(el))).toSet
    compr2.size shouldBe 1
    compr2.head shouldBe true

    val compr3: Set[Boolean] = modifiersNetworkMessagePayloads.data._2.keys.map(el => modifiersNetworkMessagePayloadsFromProto.get.data._2.keys
      .exists(id => id.sameElements(el))).toSet
    compr3.size shouldBe 1
    compr3.head shouldBe true
    val compr4 = modifiersNetworkMessagePayloads.data._2.values.map(el => modifiersNetworkMessagePayloadsFromProto.get.data._2.values
      .exists(id => id.sameElements(el))).toSet
    compr4.size shouldBe 1
    compr4.head shouldBe true

    val compr5: Set[Boolean] = modifiersNetworkMessageTransactions.data._2.keys.map(el => modifiersNetworkMessageTransactionsFromProto.get.data._2.keys
      .exists(id => id.sameElements(el))).toSet
    compr5.size shouldBe 1
    compr5.head shouldBe true
    val compr6 = modifiersNetworkMessageTransactions.data._2.values.map(el => modifiersNetworkMessageTransactionsFromProto.get.data._2.values
      .exists(id => id.sameElements(el))).toSet
    compr6.size shouldBe 1
    compr6.head shouldBe true
  }

  property("GetPeersNetworkMessage should be serialized correctly") {
    val getPeersBeforeProto: BasicMessagesRepo.GetPeersNetworkMessage.type = GetPeersNetworkMessage
    val getPeersToProto: InnerMessage = getPeersBeforeProto.toInnerMessage
    val getPeersFromProto: Option[GetPeersNetworkMessage.type] = GetPeersNetworkMessage.fromProto(getPeersToProto)

    getPeersBeforeProto.messageName shouldEqual getPeersFromProto.get.messageName

    val generalizedNetworkMessageToProto: GeneralizedNetworkProtoMessage = GeneralizedNetworkMessage.toProto(getPeersBeforeProto)
    val generalizedNetworkMessageFromProto: Try[NetworkMessage] =
      GeneralizedNetworkMessage.fromProto(ByteString(generalizedNetworkMessageToProto.toByteArray))

    generalizedNetworkMessageFromProto.isSuccess shouldBe true

    val receivedMessage: BasicMessagesRepo.GetPeersNetworkMessage.type = generalizedNetworkMessageFromProto.get match {
      case pm: BasicMessagesRepo.GetPeersNetworkMessage.type => pm
    }
    receivedMessage.messageName shouldEqual getPeersBeforeProto.messageName
  }

  property("PeersNetworkMessage should be serialized correctly") {
    val peersMessageBeforeProto: PeersNetworkMessage = PeersNetworkMessage(peers)
    val peersMessageToProto: InnerMessage = peersMessageBeforeProto.toInnerMessage
    val peersMessageFromProto: Option[PeersNetworkMessage] = PeersNetworkMessageSerializer.fromProto(peersMessageToProto)

    peersMessageFromProto.get.peers.length shouldEqual 3
    peersMessageFromProto.get.peers.contains(firstAddress) shouldEqual true
    peersMessageFromProto.get.peers.contains(secondAddress) shouldEqual true
    peersMessageFromProto.get.peers.contains(thirdAddress) shouldEqual true

    val generalizedNetworkMessageToProto: GeneralizedNetworkProtoMessage = GeneralizedNetworkMessage.toProto(peersMessageBeforeProto)
    val generalizedNetworkMessageFromProto: Try[NetworkMessage] =
      GeneralizedNetworkMessage.fromProto(ByteString(generalizedNetworkMessageToProto.toByteArray))

    generalizedNetworkMessageFromProto.isSuccess shouldBe true

    val receivedMessage: PeersNetworkMessage = generalizedNetworkMessageFromProto.get match {
      case pm: PeersNetworkMessage => pm
    }
    receivedMessage.peers.length shouldEqual 3
    receivedMessage.peers.contains(firstAddress) shouldEqual true
    receivedMessage.peers.contains(secondAddress) shouldEqual true
    receivedMessage.peers.contains(thirdAddress) shouldEqual true

    val peersMessageBeforeProtoWithoutPeers: PeersNetworkMessage = PeersNetworkMessage(Seq.empty[InetSocketAddress])
    val peersMessageToProtoWithoutPeers: InnerMessage = peersMessageBeforeProtoWithoutPeers.toInnerMessage
    val peersMessageFromProtoWithoutPeers: Option[PeersNetworkMessage] = PeersNetworkMessageSerializer.fromProto(peersMessageToProtoWithoutPeers)

    peersMessageFromProtoWithoutPeers shouldBe None

    val generalizedNetworkMessageToProtoDummy: GeneralizedNetworkProtoMessage = GeneralizedNetworkMessage.toProto(peersMessageBeforeProtoWithoutPeers)
    val generalizedNetworkMessageFromProtoDummy: Try[NetworkMessage] =
      GeneralizedNetworkMessage.fromProto(ByteString(generalizedNetworkMessageToProtoDummy.toByteArray))

    generalizedNetworkMessageFromProtoDummy.isFailure shouldBe true
  }

  property("Handshake should be serialized correctly") {
    val handshakeBeforeProtoWithAddress: Handshake = Handshake(
      protocolToBytes(settings.network.appVersion), "test-node-name", Some(firstAddress), System.currentTimeMillis())
    val handshakeToProtoWithAddress: InnerMessage = handshakeBeforeProtoWithAddress.toInnerMessage
    val handshakeFromProtoWithAddress: Option[Handshake] = HandshakeSerializer.fromProto(handshakeToProtoWithAddress)

    handshakeFromProtoWithAddress.get.declaredAddress.get shouldEqual handshakeBeforeProtoWithAddress.declaredAddress.get
    handshakeFromProtoWithAddress.get.messageName shouldEqual handshakeBeforeProtoWithAddress.messageName
    handshakeFromProtoWithAddress.get.nodeName shouldEqual handshakeBeforeProtoWithAddress.nodeName
    handshakeFromProtoWithAddress.get.time shouldEqual handshakeBeforeProtoWithAddress.time
    handshakeFromProtoWithAddress.get.protocolVersion
      .sameElements(handshakeBeforeProtoWithAddress.protocolVersion) shouldEqual true

    val handshakeBeforeProtoWithoutAddress: Handshake = Handshake(
      protocolToBytes(settings.network.appVersion), "test-node-name", None, System.currentTimeMillis())
    val handshakeToProtoWithoutAddress: InnerMessage = handshakeBeforeProtoWithoutAddress.toInnerMessage
    val handshakeFromProtoWithoutAddress: Option[Handshake] = HandshakeSerializer.fromProto(handshakeToProtoWithoutAddress)

    handshakeFromProtoWithoutAddress.get.declaredAddress shouldBe None
    handshakeFromProtoWithoutAddress.get.messageName shouldEqual handshakeBeforeProtoWithoutAddress.messageName
    handshakeFromProtoWithoutAddress.get.nodeName shouldEqual handshakeBeforeProtoWithoutAddress.nodeName
    handshakeFromProtoWithoutAddress.get.time shouldEqual handshakeBeforeProtoWithoutAddress.time
    handshakeFromProtoWithoutAddress.get.protocolVersion
      .sameElements(handshakeBeforeProtoWithoutAddress.protocolVersion) shouldEqual true

    val dummyHandshake: Handshake = Handshake(
      protocolToBytes(settings.network.appVersion), "", Some(firstAddress), System.currentTimeMillis())
    val dummyHandshakeToProto: InnerMessage = dummyHandshake.toInnerMessage
    val dummyHandshakeFromProto = HandshakeSerializer.fromProto(dummyHandshakeToProto)
    dummyHandshakeFromProto.isDefined shouldBe false

    val generalizedNetworkMessageToProto: GeneralizedNetworkProtoMessage = GeneralizedNetworkMessage.toProto(handshakeBeforeProtoWithAddress)
    val generalizedNetworkMessageFromProto: Try[NetworkMessage] =
      GeneralizedNetworkMessage.fromProto(ByteString(generalizedNetworkMessageToProto.toByteArray))

    generalizedNetworkMessageFromProto.isSuccess shouldBe true

    val receivedMessage: Handshake = generalizedNetworkMessageFromProto.get match {
      case hs: Handshake => hs
    }

    receivedMessage.declaredAddress.get shouldEqual handshakeBeforeProtoWithAddress.declaredAddress.get
    receivedMessage.messageName shouldEqual handshakeBeforeProtoWithAddress.messageName
    receivedMessage.nodeName shouldEqual handshakeBeforeProtoWithAddress.nodeName
    receivedMessage.time shouldEqual handshakeBeforeProtoWithAddress.time
    receivedMessage.protocolVersion
      .sameElements(handshakeBeforeProtoWithAddress.protocolVersion) shouldEqual true
  }
}