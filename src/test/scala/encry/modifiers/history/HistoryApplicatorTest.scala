package encry.modifiers.history

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.Timeout
import encry.EncryApp.settings
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.ModifiersToNetworkUtils
import encry.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, UpdatedHistory}
import encry.settings.{EncryAppSettings, Settings}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.{FileHelper, TestHelper}
import encry.view.{ModifiersCache, actors}
import encry.view.actors.{HistoryApplicator, StateApplicator}
import encry.view.actors.HistoryApplicator.{ModifierToHistoryAppending, StartModifiersApplicationOnStateApplicator}
import encry.view.actors.NodeViewHolder.ReceivableMessages.{LocallyGeneratedBlock, ModifierFromRemote}
import encry.view.actors.StateApplicator.NotificationAboutSuccessfullyAppliedModifier
import encry.view.history.History
import encry.view.state.{BoxHolder, UtxoState}
import encry.view.wallet.EncryWallet
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.modifiers.history.{Block, Header, HeaderProtoSerializer, Payload, PayloadProtoSerializer}
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.iq80.leveldb.Options
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.{TestActorRef, TestKit}
import org.encryfoundation.common.modifiers.PersistentModifier

import scala.concurrent.duration._

class HistoryApplicatorTest extends TestKit(ActorSystem())
  with WordSpecLike
  with ImplicitSender
  with InstanceFactory
  with Matchers
  with BeforeAndAfterAll
  with Settings {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val timeout: FiniteDuration = 5 seconds

  def utxoFromBoxHolder(boxHolder: BoxHolder, dir: File, settings: EncryAppSettings): UtxoState = {
    val storage = settings.storage.state match {
      case VersionalStorage.IODB =>
        IODBWrapper(new LSMStore(dir, keepVersions = TestNetConstants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }
    storage.insert(
      StorageVersion @@ Array.fill(32)(0: Byte),
      boxHolder.boxes.values.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes)).toList
    )
    new UtxoState(storage, settings.constants)
  }

  def blockToModifier(block: Block): (PersistentModifier, PersistentModifier) = {
    (ModifiersToNetworkUtils.fromProto(Header.modifierTypeId, HeaderProtoSerializer.toProto(block.header).toByteArray).get,
      ModifiersToNetworkUtils.fromProto(Payload.modifierTypeId, PayloadProtoSerializer.toProto(block.payload).toByteArray).get)
  }

//  "HistoryApplicator add locall generated block" should {
//    "chain synced" in {
//
//      val dir = FileHelper.getRandomTempDir
//      val history: History = generateDummyHistory(settings)
//      val wallet: EncryWallet = EncryWallet.readOrGenerate(settings.copy(directory = dir.getAbsolutePath))
//
//      val bxs = TestHelper.genAssetBoxes
//      val boxHolder = BoxHolder(bxs)
//      val state = utxoFromBoxHolder(boxHolder, FileHelper.getRandomTempDir, settings)
//
//      val nodeViewHolder = TestProbe()
//      val influx = TestProbe()
//
//      val historyBlocks = (0 until 10).foldLeft(history, Seq.empty[Block]) {
//        case ((prevHistory, blocks), _) =>
//          val block: Block = generateNextBlock(prevHistory)
//          prevHistory.append(block.header)
//          prevHistory.append(block.payload)
//          (prevHistory.reportModifierIsValid(block), blocks :+ block)
//      }
//
//      val block: Block = generateNextBlock(history)
//
//      val historyApplicator: TestActorRef[HistoryApplicator] =
//        TestActorRef[HistoryApplicator](
//          HistoryApplicator.props(history, settings, state, wallet, nodeViewHolder.ref, Some(influx.ref))
//        )
//
//      system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])
//
//      historyApplicator ! LocallyGeneratedBlock(block)
//
//      expectMsg(timeout, FullBlockChainIsSynced())
//    }
//  }

  "HistoryApplicator" should {
    "check queues" in {

      val dir = FileHelper.getRandomTempDir
      val wallet: EncryWallet = EncryWallet.readOrGenerate(settings.copy(directory = dir.getAbsolutePath))

      val bxs = TestHelper.genAssetBoxes
      val boxHolder = BoxHolder(bxs)
      val state = utxoFromBoxHolder(boxHolder, FileHelper.getRandomTempDir, settings)

      val nodeViewHolder = TestProbe()
      val influx = TestProbe()

      val initialHistory: History = generateDummyHistory(settings)

      val block1: Block = generateNextBlock(initialHistory)

      val (headerMod1, payloadMod1) = blockToModifier(block1)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(initialHistory, settings, state, wallet, nodeViewHolder.ref, Some(influx.ref))
        )

      historyApplicator ! ModifierFromRemote(headerMod1)

      ModifiersCache.put(HistoryApplicator.toKey(headerMod1.id), headerMod1, initialHistory)
      ModifiersCache.put(HistoryApplicator.toKey(payloadMod1.id), payloadMod1, initialHistory)

      println(s"modifiersQueue.size: ${historyApplicator.underlyingActor.modifiersQueue.size}")
      println(s"currentNumberOfAppliedModifiers: ${historyApplicator.underlyingActor.currentNumberOfAppliedModifiers}")
      historyApplicator.underlyingActor.modifiersQueue.size shouldBe 1
      historyApplicator.underlyingActor.currentNumberOfAppliedModifiers shouldBe 1

      historyApplicator ! StateApplicator.RequestNextModifier

      println(s"modifiersQueue.size: ${historyApplicator.underlyingActor.modifiersQueue.size}")
      println(s"currentNumberOfAppliedModifiers: ${historyApplicator.underlyingActor.currentNumberOfAppliedModifiers}")
      historyApplicator.underlyingActor.modifiersQueue.size shouldBe 0
      historyApplicator.underlyingActor.currentNumberOfAppliedModifiers shouldBe 1

//      expectMsgType[StartModifiersApplicationOnStateApplicator](timeout)

    }
  }

}
