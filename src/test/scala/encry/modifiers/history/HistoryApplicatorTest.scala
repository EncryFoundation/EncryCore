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
import encry.utils.{FileHelper, TestHelper, Utils}
import encry.utils.ChainUtils.{privKey, _}
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
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.iq80.leveldb.Options
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import akka.testkit.{TestActorRef, TestKit}
import io.circe.Decoder.state
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.encryfoundation.common.utils.Algos

import scala.concurrent.duration._

class HistoryApplicatorTest extends TestKit(ActorSystem())
  with WordSpecLike
  with ImplicitSender
  with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with Settings {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  def genHistoryBlocks(initialHistory: History, count: Int, append: Boolean): (History, Seq[Block]) = {
    (0 until count).foldLeft(initialHistory, Seq.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        if(append) {
          prevHistory.append(block.header)
          prevHistory.append(block.payload)
        }
        (prevHistory.reportModifierIsValid(block), blocks :+ block)
    }
  }

  def blockToModifier(block: Block): Seq[PersistentModifier] = Seq(block.header, block.payload)

  val numberOfInputsInOneTransaction = 1//10
  val transactionsNumberInEachBlock = 100//1000
  val numberOfOutputsInOneTransaction = 2//10
  val initialboxCount = 100//100000

  val dir: File = FileHelper.getRandomTempDir
  val wallet: EncryWallet = EncryWallet.readOrGenerate(settings.copy(directory = dir.getAbsolutePath))
  val bxs: IndexedSeq[AssetBox] = TestHelper.genAssetBoxes
  val boxHolder: BoxHolder = BoxHolder(bxs)
  val genesisBlock: Block = generateGenesisBlockValidForHistory//generateGenesisBlock(Height @@ 0)
  val state: UtxoState = utxoFromBoxHolder(boxHolder, dir, None, settings, VersionalStorage.LevelDB)
    .applyModifier(genesisBlock).right.get

  val initHistory: History = generateDummyHistory(settings)

  initHistory.append(genesisBlock.header)
  initHistory.append(genesisBlock.payload)
  val initialHistory: History = initHistory.reportModifierIsValid(genesisBlock)

  val block: Block = generateNextBlock(initialHistory)

//  val initialBoxes: IndexedSeq[AssetBox] = (0 until initialboxCount).map(nonce =>
//    genHardcodedBox(privKey.publicImage.address.address, nonce)
//  )
//  val boxesHolder: BoxHolder = BoxHolder(initialBoxes)
//  val state: UtxoState = utxoFromBoxHolder(boxesHolder, dir, None, settings, VersionalStorage.LevelDB)
//  val genesisBlock: Block = generateGenesisBlockValidForState(state)
//  val genesisState = state.applyModifier(genesisBlock).right.get
//
//  val block: Block = generateNextBlockValidForState(genesisBlock, genesisState,
//    initialBoxes.take(transactionsNumberInEachBlock * numberOfInputsInOneTransaction),
//    transactionsNumberInEachBlock, numberOfInputsInOneTransaction,
//    numberOfOutputsInOneTransaction)
//
//  val stateGenerationResults: (Vector[Block], Block, UtxoState, IndexedSeq[AssetBox]) =
//    (0 until 10).foldLeft(Vector[Block](), genesisBlock, genesisState, initialBoxes) {
//      case ((blocks, block, stateL, boxes), _) =>
//        val nextBlock: Block = generateNextBlockValidForState(block, stateL,
//          boxes.take(transactionsNumberInEachBlock * numberOfInputsInOneTransaction),
//          transactionsNumberInEachBlock, numberOfInputsInOneTransaction,
//          numberOfOutputsInOneTransaction)
//        val stateN: UtxoState = stateL.applyModifier(nextBlock).right.get
//        (blocks :+ nextBlock, nextBlock, stateN, boxes.drop(transactionsNumberInEachBlock * numberOfInputsInOneTransaction))
//    }
//
//  val chain: Vector[Block] = genesisBlock +: stateGenerationResults._1

  val nodeViewHolder = TestProbe()
  val influx = TestProbe()

  val timeout: FiniteDuration = 10 seconds

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
      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(initialHistory, settings, state, wallet, nodeViewHolder.ref, Some(influx.ref))
        )

      val modifiers: Seq[PersistentModifier] = blockToModifier(block)
      val modifierIds: Seq[ModifierId] = modifiers.map(_.id)

      modifiers.foreach(historyApplicator ! ModifierFromRemote(_))

      val modifiersQueue = historyApplicator.underlyingActor.modifiersQueue
      modifiersQueue.size shouldBe 2
      historyApplicator.underlyingActor.currentNumberOfAppliedModifiers shouldBe 2

      historyApplicator ! StateApplicator.RequestNextModifier

      historyApplicator.underlyingActor.modifiersQueue shouldBe 0
      historyApplicator.underlyingActor.currentNumberOfAppliedModifiers shouldBe 2

      val history = historyApplicator.underlyingActor.history

      expectMsgType[StartModifiersApplicationOnStateApplicator](timeout)

      assert(modifierIds.forall(history.isModifierDefined))
      assert(modifierIds.map(Algos.encode) == modifiersQueue.map(_._1))
    }
  }

  "HistoryApplicator" should {
    "check queue for rollback limit" in {

      val initHistory1: History = generateDummyHistory(settings)
      initHistory1.append(genesisBlock.header)
      initHistory1.append(genesisBlock.payload)
      val initialHistory1: History = initHistory.reportModifierIsValid(genesisBlock)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(initialHistory1, settings, state, wallet, nodeViewHolder.ref, Some(influx.ref))
            //.withDispatcher("history-applicator-dispatcher"), "historyApplicator"
        )

      val modifiers: Seq[PersistentModifier] =
        genHistoryBlocks(initialHistory, /*settings.levelDB.maxVersions + */ 10, false)._2
          .flatMap(blockToModifier)

      system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])

      modifiers.foreach(historyApplicator ! ModifierFromRemote(_))

      //      println(s"modifiersQueue: ${historyApplicator.underlyingActor.modifiersQueue.size}")
      //      println(s"currentNumberOfAppliedModifiers: ${historyApplicator.underlyingActor.currentNumberOfAppliedModifiers}")

      //      historyApplicator.underlyingActor.modifiersQueue shouldBe 0
      //      historyApplicator.underlyingActor.currentNumberOfAppliedModifiers shouldBe 2

      //val modifiersQueue = historyApplicator.underlyingActor.modifiersQueue
      //val history = historyApplicator.underlyingActor.history

      //expectMsgType[StartModifiersApplicationOnStateApplicator](timeout)

      Thread.sleep(30000)

      //expectMsg(timeout, FullBlockChainIsSynced())

      //      assert(modifierIds.forall(history.isModifierDefined))
      //      assert(modifierIds.map(Algos.encode) == modifiersQueue.map(_._1))
    }
  }

}
