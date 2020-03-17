package encry.nvg

import java.net.InetSocketAddress
import akka.actor.{ ActorRef, ActorSystem }
import akka.testkit.{ TestActorRef, TestKit, TestProbe }
import cats.syntax.eq._
import encry.consensus.HistoryConsensus.{ Equal, Older, Unknown, Younger }
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager.CheckPayloadsToDownload
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.OtherNodeSyncingStatus
import encry.nvg.NodeViewHolder.UpdateHistoryReader
import encry.nvg.Utils.instances._
import encry.settings.EncryAppSettings
import encry.view.history.{ History, HistoryReader }
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.network.BasicMessagesRepo.{ NetworkMessage, SyncInfoNetworkMessage }
import org.scalatest.{ BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike }

class NetworkMessagesProcessorTests
    extends TestKit(ActorSystem("Tested-Akka-System"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with InstanceFactory
    with OneInstancePerTest {

  "Network messages processor" should {
    "process sync info message correctly" should {
      "determine older extension" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NetworkMessagesProcessor.props(settings))

        val (historyMain: History, historyOlder: History) =
          NetworkMessagesProcessorTests.formYoungerActorState(10, 10)

        val historyReader: HistoryReader = HistoryReader(historyMain)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val syncInfoMessage: SyncInfoNetworkMessage = SyncInfoNetworkMessage(historyOlder.syncInfo)

        val (dataFromPeerMsg, address) =
          NetworkMessagesProcessorTests.formDataFromPeerMessage(syncInfoMessage, "0.0.0.0", 9001)

        val expectedResult: OtherNodeSyncingStatus = OtherNodeSyncingStatus(address, Older)

        networkProcessor ! dataFromPeerMsg

        parentActor.expectMsgPF() {
          case msg: OtherNodeSyncingStatus => msg eqv expectedResult
          case CheckPayloadsToDownload     =>
          case _                           => true shouldBe false
        }
      }
      "determine younger extension" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NetworkMessagesProcessor.props(settings))

        val (historyMain: History, historyYounger: History) =
          NetworkMessagesProcessorTests.formOlderActorState(10, 10)

        val historyReader: HistoryReader = HistoryReader(historyMain)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val syncInfoMessage: SyncInfoNetworkMessage = SyncInfoNetworkMessage(historyYounger.syncInfo)

        val (dataFromPeerMsg, address) =
          NetworkMessagesProcessorTests.formDataFromPeerMessage(syncInfoMessage, "0.0.0.0", 9001)

        val expectedResult: OtherNodeSyncingStatus = OtherNodeSyncingStatus(address, Younger)

        networkProcessor ! dataFromPeerMsg

        parentActor.expectMsgPF() {
          case msg: OtherNodeSyncingStatus => msg eqv expectedResult
          case CheckPayloadsToDownload     =>
          case _                           => true shouldBe false
        }
      }
      "determine equals extension" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NetworkMessagesProcessor.props(settings))

        val (history1: History, history2: History) =
          NetworkMessagesProcessorTests.fromEqualActorState(10, 10)

        val historyReader: HistoryReader = HistoryReader(history1)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val syncInfoMessage: SyncInfoNetworkMessage = SyncInfoNetworkMessage(history2.syncInfo)

        val (dataFromPeerMsg, address) =
          NetworkMessagesProcessorTests.formDataFromPeerMessage(syncInfoMessage, "0.0.0.0", 9001)

        val expectedResult: OtherNodeSyncingStatus = OtherNodeSyncingStatus(address, Equal)

        networkProcessor ! dataFromPeerMsg

        parentActor.expectMsgPF() {
          case msg: OtherNodeSyncingStatus => msg eqv expectedResult
          case CheckPayloadsToDownload     =>
          case _                           => true shouldBe false
        }
      }
      "determine unknown extension" in {
        val parentActor = TestProbe()

        val networkProcessor: ActorRef = parentActor.childActorOf(NetworkMessagesProcessor.props(settings))

        val (h1: History, h2: History) = NetworkMessagesProcessorTests.fromEUnknownActorState

        val historyReader: HistoryReader = HistoryReader(h1)

        networkProcessor ! UpdateHistoryReader(historyReader)

        val syncInfoMessage: SyncInfoNetworkMessage = SyncInfoNetworkMessage(h2.syncInfo)

        val (dataFromPeerMsg, address) =
          NetworkMessagesProcessorTests.formDataFromPeerMessage(syncInfoMessage, "0.0.0.0", 9001)

        val expectedResult: OtherNodeSyncingStatus = OtherNodeSyncingStatus(address, Unknown)

        networkProcessor ! dataFromPeerMsg

        parentActor.expectMsgPF() {
          case msg: OtherNodeSyncingStatus => msg eqv expectedResult
          case CheckPayloadsToDownload     =>
          case _                           => true shouldBe false
        }
      }
    }
    "process inv message correctly" in {}
    "process request for modifier correctly" in {}
    "have correct logic with check payloads to download" in {}
    "have correct logic with semantically successful modifier" in {}
  }
}

object NetworkMessagesProcessorTests extends InstanceFactory {

  def initActorState(settings: EncryAppSettings)(implicit AS: ActorSystem): TestActorRef[NetworkMessagesProcessor] = {
    val networkProcessor: TestActorRef[NetworkMessagesProcessor] =
      TestActorRef[NetworkMessagesProcessor](NetworkMessagesProcessor.props(settings))
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

  def fromEqualActorState(blocksQty: Int, olderBlocksQty: Int): (History, History) =
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

  def fromEUnknownActorState: (History, History) = {
    val history1 = (0 until 100).foldLeft(
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

}
