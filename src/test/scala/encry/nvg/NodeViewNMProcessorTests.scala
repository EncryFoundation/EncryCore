package encry.nvg

import java.net.InetSocketAddress

import akka.actor.{ ActorRef, ActorSystem }
import akka.testkit.{ TestActorRef, TestKit, TestProbe }
import cats.syntax.eq._
import cats.syntax.option._
import encry.consensus.HistoryConsensus._
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager.CheckPayloadsToDownload
import encry.network.Messages.MessageToNetwork.{ BroadcastModifier, RequestFromLocal, ResponseFromLocal, SendSyncInfo }
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.OtherNodeSyncingStatus
import encry.nvg.NodeViewHolder.{ SemanticallySuccessfulModifier, UpdateHistoryReader }
import encry.nvg.Utils.instances._
import encry.settings.EncryAppSettings
import encry.view.history.{ History, HistoryReader }
import org.encryfoundation.common.modifiers.history.{ Block, Header, Payload }
import org.encryfoundation.common.network.BasicMessagesRepo.{
  InvNetworkMessage,
  NetworkMessage,
  RequestModifiersNetworkMessage,
  SyncInfoNetworkMessage
}
import org.encryfoundation.common.utils.TaggedTypes.{ Height, ModifierId }
import org.scalatest.{ BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike }
import scorex.utils.Random

import scala.collection.immutable
import scala.concurrent.duration._

class NodeViewNMProcessorTests
    extends TestKit(ActorSystem("Tested-Akka-System"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with InstanceFactory
    with OneInstancePerTest {

  override def afterAll(): Unit = system.terminate()

  "Network messages processor" should {
    "process sync info message correctly" should {
      "determine older extension" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val (historyMain: History, historyOlder: History) =
          NodeViewNMProcessorTests.formYoungerActorState(10, 10)

        val historyReader: HistoryReader = HistoryReader(historyMain)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val syncInfoMessage: SyncInfoNetworkMessage = SyncInfoNetworkMessage(historyOlder.syncInfo)

        val (dataFromPeerMsg, address) =
          NodeViewNMProcessorTests.formDataFromPeerMessage(syncInfoMessage, "0.0.0.0", 9001)

        val expectedResult: OtherNodeSyncingStatus = OtherNodeSyncingStatus(address, Older)

        networkProcessor ! dataFromPeerMsg

        parentActor.expectMsgPF() {
          case msg: OtherNodeSyncingStatus => (msg eqv expectedResult) shouldBe true
          case CheckPayloadsToDownload     =>
        }
      }
      "determine younger extension" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val (historyMain: History, historyYounger: History) =
          NodeViewNMProcessorTests.formOlderActorState(10, 10)

        val historyReader: HistoryReader = HistoryReader(historyMain)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val syncInfoMessage: SyncInfoNetworkMessage = SyncInfoNetworkMessage(historyYounger.syncInfo)

        val (dataFromPeerMsg, address) =
          NodeViewNMProcessorTests.formDataFromPeerMessage(syncInfoMessage, "0.0.0.0", 9001)

        val expectedResult: OtherNodeSyncingStatus = OtherNodeSyncingStatus(address, Younger)

        networkProcessor ! dataFromPeerMsg

        parentActor.expectMsgPF() {
          case msg: OtherNodeSyncingStatus => (msg eqv expectedResult) shouldBe true
          case CheckPayloadsToDownload     =>
        }
      }
      "determine equals extension" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val (history1: History, history2: History) =
          NodeViewNMProcessorTests.formEqualActorState(10, 10)

        val historyReader: HistoryReader = HistoryReader(history1)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val syncInfoMessage: SyncInfoNetworkMessage = SyncInfoNetworkMessage(history2.syncInfo)

        val (dataFromPeerMsg, address) =
          NodeViewNMProcessorTests.formDataFromPeerMessage(syncInfoMessage, "0.0.0.0", 9001)

        val expectedResult: OtherNodeSyncingStatus = OtherNodeSyncingStatus(address, Equal)

        networkProcessor ! dataFromPeerMsg

        parentActor.expectMsgPF() {
          case msg: OtherNodeSyncingStatus => (msg eqv expectedResult) shouldBe true
          case CheckPayloadsToDownload     =>
        }
      }
      "determine unknown extension" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val (h1: History, h2: History) = NodeViewNMProcessorTests.formUnknownActorState

        val historyReader: HistoryReader = HistoryReader(h1)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val syncInfoMessage: SyncInfoNetworkMessage = SyncInfoNetworkMessage(h2.syncInfo)

        val (dataFromPeerMsg, address) =
          NodeViewNMProcessorTests.formDataFromPeerMessage(syncInfoMessage, "0.0.0.0", 9001)

        val expectedResult: OtherNodeSyncingStatus = OtherNodeSyncingStatus(address, Unknown)

        networkProcessor ! dataFromPeerMsg

        parentActor.expectMsgPF() {
          case msg: OtherNodeSyncingStatus => (msg eqv expectedResult) shouldBe true
          case CheckPayloadsToDownload     =>
        }
      }
      "determine fork extension" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val (h1: History, h2: History) =
          NodeViewNMProcessorTests.formForkActorState(10, 20, 5)

        val historyReader: HistoryReader = HistoryReader(h1)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val syncInfoMessage: SyncInfoNetworkMessage = SyncInfoNetworkMessage(h2.syncInfo)

        val (dataFromPeerMsg, address) =
          NodeViewNMProcessorTests.formDataFromPeerMessage(syncInfoMessage, "0.0.0.0", 9001)

        val expectedResult: OtherNodeSyncingStatus = OtherNodeSyncingStatus(address, Fork)

        networkProcessor ! dataFromPeerMsg

        parentActor.expectMsgPF() {
          case msg: OtherNodeSyncingStatus => (msg eqv expectedResult) shouldBe true
          case CheckPayloadsToDownload     =>
        }
      }
    }
    "process inv message correctly" should {
      "not process inv for payload while full chain is not synced" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val ids: immutable.IndexedSeq[ModifierId] = (0 to 10).map(_ => ModifierId @@ Random.randomBytes())

        val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9001)

        networkProcessor ! DataFromPeer(InvNetworkMessage(Payload.modifierTypeId -> ids), address)

        parentActor.expectNoMsg()
      }
      "not create response from local for payloads if header's chain is not synced" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val ids: immutable.IndexedSeq[ModifierId] = (0 to 10).map(_ => ModifierId @@ Random.randomBytes())

        val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9001)

        val historyReader = HistoryReader.empty

        historyReader.isHeadersChainSyncedVar = false

        historyReader.isFullChainSynced = true

        networkProcessor ! UpdateHistoryReader(historyReader)

        networkProcessor ! DataFromPeer(InvNetworkMessage(Payload.modifierTypeId -> ids), address)

        parentActor.expectNoMsg()
      }
      "create response from local for payloads if header's chain is synced" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val ids: immutable.IndexedSeq[ModifierId] = (0 to 10).map(_ => ModifierId @@ Random.randomBytes())

        val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9001)

        val historyReader = HistoryReader.empty

        historyReader.isHeadersChainSyncedVar = true

        historyReader.isFullChainSynced = true

        networkProcessor ! UpdateHistoryReader(historyReader)

        networkProcessor ! DataFromPeer(InvNetworkMessage(Header.modifierTypeId -> ids), address)

        val requiredRequestFromLocal = RequestFromLocal(address.some, Header.modifierTypeId, ids.toList)

        parentActor.expectMsgPF() {
          case CheckPayloadsToDownload =>
          case msg: RequestFromLocal   => (msg eqv requiredRequestFromLocal) shouldBe true
        }
      }
      "request only unique new modifiers" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val ids: immutable.IndexedSeq[ModifierId] = (0 to 10).map(_ => ModifierId @@ Random.randomBytes())

        val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9001)

        val (history, blocks) = NodeViewNMProcessorTests.formHistory

        val historyReader = HistoryReader(history)

        historyReader.isHeadersChainSyncedVar = true

        historyReader.isFullChainSynced = true

        networkProcessor ! UpdateHistoryReader(historyReader)

        networkProcessor ! DataFromPeer(InvNetworkMessage(Header.modifierTypeId -> (ids ++ blocks.map(_.id))), address)

        val requiredRequestFromLocal = RequestFromLocal(address.some, Header.modifierTypeId, ids.toList)

        parentActor.expectMsgPF() {
          case CheckPayloadsToDownload =>
          case msg: RequestFromLocal   => (msg eqv requiredRequestFromLocal) shouldBe true
        }
      }
    }
    "process semantically successful modifier correctly" should {
      "update local cache with last semantically successful modifier" in {
        val networkMessagesProcessor: TestActorRef[NodeViewNMProcessor] =
          NodeViewNMProcessorTests.initActorState(settings)

        val reader = HistoryReader.empty

        reader.isFullChainSynced = true

        networkMessagesProcessor ! UpdateHistoryReader(reader)

        val block = generateGenesisBlock(Height @@ 1)

        networkMessagesProcessor ! SemanticallySuccessfulModifier(block)

        networkMessagesProcessor.underlyingActor.modifiersRequestCache.size shouldBe 2
        networkMessagesProcessor.underlyingActor.modifiersRequestCache.get(block.encodedId).nonEmpty shouldBe true
        networkMessagesProcessor.underlyingActor.modifiersRequestCache
          .get(block.payload.encodedId)
          .nonEmpty shouldBe true

        val block2 = generateGenesisBlock(Height @@ 2)

        networkMessagesProcessor ! SemanticallySuccessfulModifier(block2)

        networkMessagesProcessor.underlyingActor.modifiersRequestCache.size shouldBe 2
        networkMessagesProcessor.underlyingActor.modifiersRequestCache.get(block.encodedId).nonEmpty shouldBe false
        networkMessagesProcessor.underlyingActor.modifiersRequestCache
          .get(block.payload.encodedId)
          .nonEmpty shouldBe false

        networkMessagesProcessor.underlyingActor.modifiersRequestCache.get(block2.encodedId).nonEmpty shouldBe true
        networkMessagesProcessor.underlyingActor.modifiersRequestCache
          .get(block2.payload.encodedId)
          .nonEmpty shouldBe true
      }
      "send broadcast message for new modifier" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val reader = HistoryReader.empty

        reader.isFullChainSynced = true

        networkProcessor ! UpdateHistoryReader(reader)

        val block = generateGenesisBlock(Height @@ 1)

        networkProcessor ! SemanticallySuccessfulModifier(block)

        parentActor.expectMsgPF() {
          case CheckPayloadsToDownload =>
          case BroadcastModifier(modType, id) if modType == Header.modifierTypeId =>
            id.sameElements(block.id) shouldBe true
          case BroadcastModifier(modType, id) if modType == Payload.modifierTypeId =>
            id.sameElements(block.payload.id) shouldBe true
        }
      }
    }
    "process request for modifier correctly" should {
      "response for modifiers which are in cache by using this cache" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val (history, blocks) = NodeViewNMProcessorTests.formHistory

        val historyReader = HistoryReader(history)

        historyReader.isFullChainSynced = true

        val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9001)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val block = generateGenesisBlock(Height @@ 1)

        networkProcessor ! SemanticallySuccessfulModifier(block)

        networkProcessor ! DataFromPeer(
          RequestModifiersNetworkMessage(Header.modifierTypeId -> (blocks.headOption.get.id :: block.id :: Nil)),
          address
        )

        parentActor.expectMsgPF() {
          case ResponseFromLocal(_, _, ids) if ids.size == 1 =>
            ids.forall {
              case (id, _) =>
                id.sameElements(block.id)
            } shouldBe true
          case ResponseFromLocal(_, _, ids) =>
            ids.forall {
              case (id, _) =>
                id.sameElements(blocks.headOption.get.id)
            } shouldBe true
          case CheckPayloadsToDownload =>
          case _: BroadcastModifier    =>
        }
      }
      "response for headers in 1 message for all headers" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val (history, blocks) = NodeViewNMProcessorTests.formHistory

        val historyReader = HistoryReader(history)

        historyReader.isFullChainSynced = true

        val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9001)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val block = generateGenesisBlock(Height @@ 1)

        networkProcessor ! SemanticallySuccessfulModifier(block)

        networkProcessor ! DataFromPeer(
          RequestModifiersNetworkMessage(Header.modifierTypeId -> blocks.take(2).map(_.id)),
          address
        )

        parentActor.expectMsgPF() {
          case ResponseFromLocal(_, _, ids) if ids.size == 2 =>
            ids.keys.toList.zip(blocks.take(2).map(_.id)).forall { case (id, id1) => id.sameElements(id1) } shouldBe true
          case CheckPayloadsToDownload =>
          case _: BroadcastModifier    =>
        }
      }
      "response for payloads in 1 message for 1 payload" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val (history, blocks) = NodeViewNMProcessorTests.formHistory

        val historyReader = HistoryReader(history)

        historyReader.isFullChainSynced = true

        val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9001)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val block = generateGenesisBlock(Height @@ 1)

        networkProcessor ! SemanticallySuccessfulModifier(block)

        networkProcessor ! DataFromPeer(
          RequestModifiersNetworkMessage(Payload.modifierTypeId -> blocks.take(2).map(_.payload.id)),
          address
        )

        parentActor.expectMsgPF() {
          case ResponseFromLocal(_, _, ids) if ids.size == 1 =>
            ids.keys.toList.forall { id =>
              id.sameElements(blocks(1).payload.id) || id.sameElements(blocks.head.payload.id)
            } shouldBe true
          case CheckPayloadsToDownload =>
          case _: BroadcastModifier    =>
        }
      }
    }
    "have correct logic with check payloads to download" should {
      "request required modifiers" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val (_, blocks) = NodeViewNMProcessorTests.formHistory

        val history = blocks.take(5).foldLeft(generateDummyHistory(settings)) {
          case (h, block) =>
            h.append(block.header).right.get._1.reportModifierIsValid(block.header)
        }

        history.isFullChainSynced = true

        val historyReader = HistoryReader(history)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val requiredResponse = RequestFromLocal(none, Payload.modifierTypeId, blocks.take(5).map(_.payload.id))

        parentActor.expectMsgPF(settings.network.syncInterval + 1.seconds) {
          case _: SendSyncInfo       =>
          case msg: RequestFromLocal => msg.eqv(requiredResponse) shouldBe true
        }

        parentActor.expectMsgPF(settings.network.syncInterval + 1.seconds) {
          case _: SendSyncInfo       =>
          case msg: RequestFromLocal => msg.eqv(requiredResponse) shouldBe true
        }

        val updHistory = blocks.drop(5).foldLeft(history) {
          case (h, block) =>
            h.append(block.header).right.get._1.reportModifierIsValid(block.header)
        }

        val updHistory2 = blocks.take(3).foldLeft(updHistory) {
          case (h, block) =>
            h.append(block.payload).right.get._1.reportModifierIsValid(block.payload).reportModifierIsValid(block)
        }

        val historyReader1 = HistoryReader(updHistory2)

        networkProcessor ! UpdateHistoryReader(historyReader1)

        val requiredResponse1 = RequestFromLocal(none, Payload.modifierTypeId, blocks.drop(3).map(_.payload.id))

        parentActor.expectMsgPF(settings.network.syncInterval + 1.seconds) {
          case _: SendSyncInfo       =>
          case msg: RequestFromLocal => msg.eqv(requiredResponse1) shouldBe true
        }
      }
      "not request if headers chain is not synced" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NodeViewNMProcessor.props(settings))

        val (_, blocks) = NodeViewNMProcessorTests.formHistory

        val history = blocks.foldLeft(generateDummyHistory(settings)) {
          case (h, block) =>
            h.append(block.header).right.get._1.reportModifierIsValid(block.header)
        }

        history.isFullChainSynced = false

        val historyReader = HistoryReader(history)

        networkProcessor ! UpdateHistoryReader(historyReader)

        parentActor.expectMsgPF(settings.network.syncInterval + 2.seconds) {
          case _: SendSyncInfo =>
        }
      }
    }
  }
}

object NodeViewNMProcessorTests extends InstanceFactory {

  def initActorState(settings: EncryAppSettings)(implicit AS: ActorSystem): TestActorRef[NodeViewNMProcessor] = {
    val networkProcessor: TestActorRef[NodeViewNMProcessor] =
      TestActorRef[NodeViewNMProcessor](NodeViewNMProcessor.props(settings))
    networkProcessor
  }

  def formDataFromPeerMessage(innerMessage: NetworkMessage, host: String, port: Int)(
    implicit AS: ActorSystem
  ): (DataFromPeer, InetSocketAddress) = {
    val address = new InetSocketAddress(host, port)
    DataFromPeer(innerMessage, address) -> address
  }

  def formYoungerActorState(blocksQty: Int, olderBlocksQty: Int): (History, History) = {
    val (hMain, hOlder, blocks) = (0 until blocksQty).foldLeft(
      generateDummyHistory(settings),
      generateDummyHistory(settings),
      List.empty[Block]
    ) {
      case ((historyMain, historyOlder, blocks: List[Block]), _) =>
        val block: Block = generateNextBlock(historyMain)
        val hMain: History = historyMain
          .append(block.header)
          .right
          .get
          ._1
          .append(block.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block)

        val hOlder = historyOlder
          .append(block.header)
          .right
          .get
          ._1
          .append(block.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block)
        (hMain, hOlder, block +: blocks)
    }
    val (hOlderUpdated, _) = (0 until olderBlocksQty).foldLeft(hOlder, List.empty[Block]) {
      case ((historyOlder, blocks: List[Block]), _) =>
        val block: Block = generateNextBlock(historyOlder)
        val hOlder: History = historyOlder
          .append(block.header)
          .right
          .get
          ._1
          .append(block.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block)
        (hOlder, block +: blocks)
    }
    (hMain, hOlderUpdated)
  }

  def formOlderActorState(blocksQty: Int, olderBlocksQty: Int): (History, History) = {
    val (historyYounger, historyOlder) = formYoungerActorState(blocksQty, olderBlocksQty)
    (historyOlder, historyYounger)
  }

  def formEqualActorState(blocksQty: Int, olderBlocksQty: Int): (History, History) =
    (0 until blocksQty).foldLeft(
      generateDummyHistory(settings),
      generateDummyHistory(settings)
    ) {
      case ((historyMain, historyOlder), _) =>
        val block: Block = generateNextBlock(historyMain)
        val hEq1: History = historyMain
          .append(block.header)
          .right
          .get
          ._1
          .append(block.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block)

        val hEq2 = historyOlder
          .append(block.header)
          .right
          .get
          ._1
          .append(block.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block)
        (hEq1, hEq2)
    }

  def formUnknownActorState: (History, History) = {
    val history1 = (0 until 10).foldLeft(
      generateDummyHistory(settings)
    ) {
      case (h1, _) =>
        val block1: Block = generateNextBlock(h1)
        val hEq1: History = h1
          .append(block1.header)
          .right
          .get
          ._1
          .append(block1.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block1)
        hEq1
    }
    val history2 = (0 until 2).foldLeft(
      generateDummyHistory(settings)
    ) {
      case (h2, _) =>
        val block1: Block = generateNextBlock(h2)
        val hEq2: History = h2
          .append(block1.header)
          .right
          .get
          ._1
          .append(block1.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block1)
        hEq2
    }
    history1 -> history2
  }

  def formForkActorState(forkOn: Int, forkSize: Int, mainChainSize: Int): (History, History) = {
    val (h1, h2) = (0 until forkOn).foldLeft(
      generateDummyHistory(settings),
      generateDummyHistory(settings)
    ) {
      case ((historyMain, historyOlder), _) =>
        val block: Block = generateNextBlock(historyMain)
        val hEq1: History = historyMain
          .append(block.header)
          .right
          .get
          ._1
          .append(block.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block)

        val hEq2 = historyOlder
          .append(block.header)
          .right
          .get
          ._1
          .append(block.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block)
        (hEq1, hEq2)
    }
    val fork = (0 until forkSize).foldLeft(h1) {
      case (historyMain, _) =>
        val block: Block = generateNextBlock(historyMain)
        val hEq1: History = historyMain
          .append(block.header)
          .right
          .get
          ._1
          .append(block.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block)
        hEq1
    }
    val mch = (0 until mainChainSize).foldLeft(h2) {
      case (historyMain, _) =>
        val block: Block = generateNextBlock(historyMain)
        val hEq1: History = historyMain
          .append(block.header)
          .right
          .get
          ._1
          .append(block.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block)
        hEq1
    }
    mch -> fork
  }

  def formHistory: (History, List[Block]) =
    (0 to 10).foldLeft(generateDummyHistory(settings), List.empty[Block]) {
      case ((historyMain, blocks), _) =>
        val block: Block = generateNextBlock(historyMain)
        val hEq1: History = historyMain
          .append(block.header)
          .right
          .get
          ._1
          .append(block.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(block)
        hEq1 -> (blocks :+ block)
    }
}
