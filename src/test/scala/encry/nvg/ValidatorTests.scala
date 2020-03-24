//package encry.nvg
//
//import java.net.InetSocketAddress
//
//import akka.actor.{ ActorRef, ActorSystem }
//import akka.testkit.{ TestKit, TestProbe }
//import encry.modifiers.InstanceFactory
//import encry.network.BlackList.BanReason.{
//  CorruptedSerializedBytes,
//  ModifierIdInTheNetworkMessageIsNotTheSameAsIdOfModifierInThisMessage,
//  PreSemanticInvalidModifier,
//  SyntacticallyInvalidPersistentModifier
//}
//import encry.network.PeersKeeper.BanPeer
//import encry.nvg.ModifiersValidator.{ InvalidModifierBytes, ModifierForValidation, ValidatedModifier }
//import encry.nvg.NodeViewHolder.SyntacticallyFailedModification
//import encry.view.history.HistoryReader
//import org.encryfoundation.common.modifiers.history.{ Block, Header, HeaderProtoSerializer }
//import org.encryfoundation.common.utils.TaggedTypes.ModifierId
//import org.scalatest.{ BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike }
//import scorex.utils.Random
//
//class ValidatorTests
//    extends TestKit(ActorSystem("Tested-Akka-System"))
//    with WordSpecLike
//    with Matchers
//    with BeforeAndAfterAll
//    with InstanceFactory
//    with OneInstancePerTest {
//
//  override def afterAll(): Unit = system.terminate()
//
//  "Modifiers validator" should {
//    "notify intermediary actor about modifier with invalid raw bytes" in {
//      val nvh = TestProbe()
//
//      val intermediary = TestProbe()
//
//      val parentActor = TestProbe()
//
//      val networkProcessor: ActorRef =
//        parentActor.childActorOf(ModifiersValidator.props(nvh.ref, intermediary.ref, settings))
//
//      val (history, blocks) = NodeViewNMProcessorTests.formHistory
//
//      val reader = HistoryReader(history)
//
//      val corruptedBytes = Random.randomBytes(190)
//
//      val remote = new InetSocketAddress("0.0.0.0", 9001)
//
//      val randomId = ModifierId @@ Random.randomBytes()
//
//      networkProcessor ! ModifierForValidation(reader, randomId, Header.modifierTypeId, corruptedBytes, remote)
//
//      intermediary.expectMsgPF() {
//        case BanPeer(r, CorruptedSerializedBytes) => r shouldBe remote
//        case InvalidModifierBytes(id)             => id.sameElements(blocks.head.id) shouldBe true
//      }
//    }
//    "notify intermediary actor about pre semantic invalid modifier" in {
//      val nvh = TestProbe()
//
//      val intermediary = TestProbe()
//
//      val parentActor = TestProbe()
//
//      val networkProcessor: ActorRef =
//        parentActor.childActorOf(ModifiersValidator.props(nvh.ref, intermediary.ref, settings))
//
//      val (history, blocks) = NodeViewNMProcessorTests.formHistory
//
//      val reader = HistoryReader(history)
//
//      val corruptedBlock = blocks.head.copy(
//        header = blocks.head.header.copy(height = -1000)
//      )
//
//      val corruptedBytes = HeaderProtoSerializer.toProto(corruptedBlock.header).toByteArray
//
//      val remote = new InetSocketAddress("0.0.0.0", 9001)
//
//      networkProcessor ! ModifierForValidation(reader,
//                                               ModifierId @@ Random.randomBytes(),
//                                               Header.modifierTypeId,
//                                               corruptedBytes,
//                                               remote)
//
//      intermediary.expectMsgPF() {
//        case BanPeer(r, PreSemanticInvalidModifier(_)) => r shouldBe remote
//        case SyntacticallyFailedModification(mod, _)   => mod.id.sameElements(blocks.head.id) shouldBe true
//      }
//    }
//    "notify intermediary actor about syntactically invalid modifier" in {
//      val nvh = TestProbe()
//
//      val intermediary = TestProbe()
//
//      val parentActor = TestProbe()
//
//      val networkProcessor: ActorRef =
//        parentActor.childActorOf(ModifiersValidator.props(nvh.ref, intermediary.ref, settings))
//
//      val (history, blocks) = NodeViewNMProcessorTests.formHistory
//
//      val reader = HistoryReader(history)
//
//      val corruptedBlock = blocks.head.copy(
//        header = blocks.head.header.copy(parentId = ModifierId @@ blocks.head.header.id.drop(2))
//      )
//
//      val corruptedBytes = HeaderProtoSerializer.toProto(corruptedBlock.header).toByteArray
//
//      val remote = new InetSocketAddress("0.0.0.0", 9001)
//
//      networkProcessor ! ModifierForValidation(reader,
//                                               ModifierId @@ Random.randomBytes(),
//                                               Header.modifierTypeId,
//                                               corruptedBytes,
//                                               remote)
//
//      intermediary.expectMsgPF() {
//        case BanPeer(r, SyntacticallyInvalidPersistentModifier) => r shouldBe remote
//        case SyntacticallyFailedModification(mod, _)            => mod.id.sameElements(blocks.head.id) shouldBe true
//      }
//    }
//    "notify intermediary about incorrect modifier id" in {
//      val nvh = TestProbe()
//
//      val intermediary = TestProbe()
//
//      val parentActor = TestProbe()
//
//      val networkProcessor: ActorRef =
//        parentActor.childActorOf(ModifiersValidator.props(nvh.ref, intermediary.ref, settings))
//
//      val (history, blocks) = NodeViewNMProcessorTests.formHistory
//
//      val reader = HistoryReader(history)
//
//      val corruptedBytes = HeaderProtoSerializer.toProto(blocks.head.header).toByteArray
//
//      val remote = new InetSocketAddress("0.0.0.0", 9001)
//
//      networkProcessor ! ModifierForValidation(reader,
//                                               ModifierId @@ Random.randomBytes(),
//                                               Header.modifierTypeId,
//                                               corruptedBytes,
//                                               remote)
//
//      intermediary.expectMsgPF() {
//        case BanPeer(r, ModifierIdInTheNetworkMessageIsNotTheSameAsIdOfModifierInThisMessage) => r shouldBe remote
//        case SyntacticallyFailedModification(mod, _)                                          => mod.id.sameElements(blocks.head.id) shouldBe true
//      }
//    }
//    "notify nvh actor about valid modifier" in {
//      val nvh = TestProbe()
//
//      val intermediary = TestProbe()
//
//      val parentActor = TestProbe()
//
//      val networkProcessor: ActorRef =
//        parentActor.childActorOf(ModifiersValidator.props(nvh.ref, intermediary.ref, settings))
//
//      val (history, _) = NodeViewNMProcessorTests.formHistory
//
//      val reader = HistoryReader(history)
//
//      val correctBlock: Block = generateNextBlock(history)
//
//      val correctBytes = HeaderProtoSerializer.toProto(correctBlock.header).toByteArray
//
//      val remote = new InetSocketAddress("0.0.0.0", 9001)
//
//      networkProcessor ! ModifierForValidation(reader, correctBlock.id, Header.modifierTypeId, correctBytes, remote)
//
//      nvh.expectMsgPF() {
//        case ValidatedModifier(mod) => mod.id.sameElements(correctBlock.id) shouldBe true
//      }
//    }
//  }
//
//}
