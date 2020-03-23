package encry.nvg

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.consensus.EncrySupplyController
import encry.modifiers.InstanceFactory
import encry.modifiers.mempool.TransactionFactory
import encry.network.Messages.MessageToNetwork.{BroadcastModifier, RequestFromLocal, SendSyncInfo}
import encry.network.NetworkController.ReceivableMessages.RegisterMessagesHandler
import encry.network.NetworkRouter.{ModifierFromNetwork, RegisterForModsHandling}
import encry.nvg.NodeViewHolder.SemanticallySuccessfulModifier
import encry.utils.implicits.UTXO.{combineAll, _}
import encry.utils.{FileHelper, Mnemonic, NetworkTimeProvider, TestHelper}
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.view.wallet.AccountManager
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

import scala.concurrent.duration._

class PipelinesTests
    extends TestKit(ActorSystem("Tested-Akka-System"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with InstanceFactory
    with OneInstancePerTest
    with StrictLogging {

  override def afterAll(): Unit = system.terminate()

  "Node view pipelines" should {
    "correct process modifier from the network async" in {
      val tmpFile = FileHelper.getRandomTempDir
      val path    = tmpFile.getAbsolutePath
      val settingsWithNewPath =
        settings
          .copy(directory = path)
          .copy(wallet = settings.wallet.map(_.copy(password = "123")))
          .copy(node = settings.node.copy(isTestMod = true))
      AccountManager.init(Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16)),
                          "123",
                          settingsWithNewPath)

      val intermediaryParent                = TestProbe()
      val networkIntermediary               = TestProbe()
      val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settingsWithNewPath.ntp)

      val intermediary: ActorRef = intermediaryParent.childActorOf(
        IntermediaryNVH.props(
          settingsWithNewPath,
          networkIntermediary.ref,
          timeProvider,
          None,
          TestProbe().ref,
          TestProbe().ref
        )
      )

      val remote = new InetSocketAddress("0.0.0.0", 9001)

      val (_, _, blocks) = PipelinesTests.generateValidForHistoryAndStateBlocks(300,
                                                                                generateDummyHistory(settings),
                                                                                UtxoState.genesis(
                                                                                  FileHelper.getRandomTempDir,
                                                                                  FileHelper.getRandomTempDir,
                                                                                  settings,
                                                                                  None
                                                                                ))

      blocks.reverse.foreach { block =>
        intermediary ! ModifierFromNetwork(
          remote,
          Header.modifierTypeId,
          block.id,
          HeaderProtoSerializer.toProto(block.header).toByteArray
        )
        logger.info(s"Sent to nvh actor header ${block.encodedId}")
      }

      Thread.sleep(8000)

      networkIntermediary.expectMsgPF(15.seconds) {
        case SemanticallySuccessfulModifier(mod) =>
          blocks.exists(_.id.sameElements(mod.id)) shouldBe true
        case msg @ SendSyncInfo(_) =>
        case msg @ BroadcastModifier(modifierTypeId, modifierId) =>
          blocks.exists(_.id.sameElements(modifierId)) shouldBe true
        case RegisterMessagesHandler(_, _) =>
        case RegisterForModsHandling       =>
        case RequestFromLocal(s, m, mods) =>
          mods.size shouldBe blocks.size
          mods.zip(blocks).forall {
            case (id, block) => id.sameElements(block.id)
          } shouldBe true
      }

      blocks.reverse.foreach { block =>
        intermediary ! ModifierFromNetwork(
          remote,
          Payload.modifierTypeId,
          block.payload.id,
          PayloadProtoSerializer.toProto(block.payload).toByteArray
        )
      }

      Thread.sleep(10000)

      networkIntermediary.expectMsgPF(15.seconds) {
        case SemanticallySuccessfulModifier(mod) =>
          blocks.exists(_.id.sameElements(mod.id)) shouldBe true
        case msg @ SendSyncInfo(_)   =>
        case RegisterForModsHandling =>
        case msg @ BroadcastModifier(modifierTypeId, modifierId) =>
          blocks.exists(_.id.sameElements(modifierId)) shouldBe true
        case RegisterMessagesHandler(_, _) =>
        case RequestFromLocal(s, m, mods)  =>
      }

    }
    "correct process modifier from the network sync" in {
      val tmpFile = FileHelper.getRandomTempDir
      val path    = tmpFile.getAbsolutePath
      val settingsWithNewPath =
        settings
          .copy(directory = path)
          .copy(wallet = settings.wallet.map(_.copy(password = "123")))
          .copy(node = settings.node.copy(isTestMod = true))
      AccountManager.init(Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16)),
                          "123",
                          settingsWithNewPath)

      val networkIntermediary               = TestProbe()
      val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settingsWithNewPath.ntp)

      val intermediary = TestActorRef[IntermediaryNVH](
        IntermediaryNVH.props(
          settingsWithNewPath,
          networkIntermediary.ref,
          timeProvider,
          None,
          TestProbe().ref,
          TestProbe().ref
        )
      )

      val remote = new InetSocketAddress("0.0.0.0", 9001)

      val (_, _, blocks) = PipelinesTests.generateValidForHistoryAndStateBlocks(300,
                                                                                generateDummyHistory(settings),
                                                                                UtxoState.genesis(
                                                                                  FileHelper.getRandomTempDir,
                                                                                  FileHelper.getRandomTempDir,
                                                                                  settings,
                                                                                  None
                                                                                ))

      blocks.reverse.foreach { block =>
        intermediary ! ModifierFromNetwork(
          remote,
          Header.modifierTypeId,
          block.id,
          HeaderProtoSerializer.toProto(block.header).toByteArray
        )
        logger.info(s"Sent to nvh actor header ${block.encodedId}")
      }

      Thread.sleep(8000)

      intermediary.underlyingActor.historyReader.getBestHeaderHeight shouldBe 300
      intermediary.underlyingActor.historyReader.getBestBlockHeight shouldBe -1

      blocks.reverse.foreach { block =>
        intermediary ! ModifierFromNetwork(
          remote,
          Payload.modifierTypeId,
          block.payload.id,
          PayloadProtoSerializer.toProto(block.payload).toByteArray
        )
      }

      Thread.sleep(10000)

      intermediary.underlyingActor.historyReader.getBestHeaderHeight shouldBe 300
      intermediary.underlyingActor.historyReader.getBestBlockHeight shouldBe 300
    }
    "work with forks correctly" in {
      logger.info("================ work with forks correctly start ====================")
      val tmpFile = FileHelper.getRandomTempDir
      val path    = tmpFile.getAbsolutePath
      val settingsWithNewPath =
        settings
          .copy(directory = path)
          .copy(wallet = settings.wallet.map(_.copy(password = "123")))
          .copy(node = settings.node.copy(isTestMod = true))

      AccountManager.init(Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16)),
                          "123",
                          settingsWithNewPath)

      val networkIntermediary               = TestProbe()
      val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settingsWithNewPath.ntp)

      val intermediary = TestActorRef[IntermediaryNVH](
        IntermediaryNVH.props(
          settingsWithNewPath,
          networkIntermediary.ref,
          timeProvider,
          None,
          TestProbe().ref,
          TestProbe().ref
        )
      )

      val remote = new InetSocketAddress("0.0.0.0", 9001)

      val ((h1, s1, b1), (h2, s2, b2)) = PipelinesTests.genForOn(5)

      b1.foreach { block =>
        intermediary ! ModifierFromNetwork(
          remote,
          Header.modifierTypeId,
          block.header.id,
          HeaderProtoSerializer.toProto(block.header).toByteArray
        )
      }

      Thread.sleep(3000)

      b1.foreach { block =>
        intermediary ! ModifierFromNetwork(
          remote,
          Payload.modifierTypeId,
          block.payload.id,
          PayloadProtoSerializer.toProto(block.payload).toByteArray
        )
      }

      Thread.sleep(5000)

      b2.foreach { block =>
        intermediary ! ModifierFromNetwork(
          remote,
          Header.modifierTypeId,
          block.header.id,
          HeaderProtoSerializer.toProto(block.header).toByteArray
        )
      }

      Thread.sleep(3000)

      b2.foreach { block =>
        intermediary ! ModifierFromNetwork(
          remote,
          Payload.modifierTypeId,
          block.payload.id,
          PayloadProtoSerializer.toProto(block.payload).toByteArray
        )
      }

      Thread.sleep(5000)

      println("getBestBlock = " + Algos.encode(intermediary.underlyingActor.historyReader.getBestBlock.get.id))
      println("b2 = " + Algos.encode(b2.last.id))
      intermediary.underlyingActor.historyReader.getBestBlock.get.id.sameElements(b2.last.id) shouldBe true
      intermediary.underlyingActor.historyReader.getBestHeader.get.id.sameElements(b2.last.id) shouldBe true

    }
  }

}

object PipelinesTests extends InstanceFactory with StrictLogging {

  val key: PrivateKey25519 = TestHelper.genKeys(1).head

  def generateValidForHistoryAndStateBlocks(
    blocksQty: Int,
    history: History,
    utxo: UtxoState,
    from: Int = 0
  ): (History, UtxoState, List[Block]) = {
    (from to from + blocksQty).foldLeft(
      history,
      utxo,
      List.empty[Block]
    ) {
      case ((history, state, blocks), i) =>
        val blockNext: Block =
          if (i > 0) {
            val boxes: Seq[AssetBox] =
              history.getBestBlock.get.payload.txs.flatMap(_.newBoxes.toList).take(30).collect {
                case a: AssetBox if a.amount > 13 => a
              }
            val txs: Vector[Transaction] =
              generatePaymentTransactions(key, boxes.toIndexedSeq, 1, 2)
            val feesTotal   = txs.map(_.fee).sum
            val supplyTotal = EncrySupplyController.supplyAt(Height @@ i, settings.constants)
            val coinbase: Transaction = TransactionFactory
              .coinbaseTransactionScratch(key.publicImage, timestamp, supplyTotal, feesTotal, Height @@ i)
            val resTxs = txs :+ coinbase
            val difficulty: Difficulty = history.getBestHeader
              .map(
                parent =>
                  history.requiredDifficultyAfter(parent) match {
                    case Right(value) => value
                    case Left(value)  => EncryApp.forceStopApplication(999, value.toString)
                }
              )
              .getOrElse(TestNetConstants.InitialDifficulty)
            val combinedStateChange: UtxoState.StateChange = combineAll(resTxs.map(UtxoState.tx2StateChange).toList)
            val newStateRoot = state.tree
              .getOperationsRootHash(
                combinedStateChange.outputsToDb.toList,
                combinedStateChange.inputsToDb.toList
              )
              .get

            val header =
              Header(
                TestNetConstants.Version,
                history.getBestHeaderId.get,
                Payload.rootHash(resTxs.map(_.id)),
                System.currentTimeMillis(),
                i,
                1,
                difficulty,
                EquihashSolution(Seq(1, 3)),
                newStateRoot
              )
            val payload = Payload(header.id, resTxs)
            val block   = Block(header, payload)
            block
          } else {
            val supplyTotal = EncrySupplyController.supplyAt(Height @@ i, settings.constants)
            val coinbase: Transaction = TransactionFactory
              .coinbaseTransactionScratch(key.publicImage, timestamp, supplyTotal, 0, Height @@ i)
            val resTxs = List(coinbase)
            val difficulty: Difficulty = history.getBestHeader
              .map(
                parent =>
                  history.requiredDifficultyAfter(parent) match {
                    case Right(value) => value
                    case Left(value)  => EncryApp.forceStopApplication(999, value.toString)
                }
              )
              .getOrElse(TestNetConstants.InitialDifficulty)
            val combinedStateChange: UtxoState.StateChange = combineAll(resTxs.map(UtxoState.tx2StateChange).toList)
            val newStateRoot = state.tree
              .getOperationsRootHash(
                combinedStateChange.outputsToDb.toList,
                combinedStateChange.inputsToDb.toList
              )
              .get

            val header =
              Header(
                0: Byte,
                Header.GenesisParentId,
                Payload.rootHash(resTxs.map(_.id)),
                System.currentTimeMillis(),
                i,
                1,
                difficulty,
                EquihashSolution(Seq(1, 3)),
                newStateRoot
              )
            val payload = Payload(header.id, resTxs)
            val block   = Block(header, payload)
            block
          }
        val h = history
          .append(blockNext.header)
          .right
          .get
          ._1
          .append(blockNext.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(blockNext)

        val s = state
          .applyModifier(blockNext.header)
          .right
          .get
          .applyModifier(blockNext)
          .right
          .get
        (h, s, blocks :+ blockNext)
    }
  }

  def genForOn(
    blocksQty: Int
  ): ((History, UtxoState, List[Block]), (History, UtxoState, List[Block])) = {
    val (h, s, h1, s1, b) = (0 to blocksQty).foldLeft(
      generateDummyHistory(settings),
      UtxoState.genesis(
        FileHelper.getRandomTempDir,
        FileHelper.getRandomTempDir,
        settings,
        None
      ),
      generateDummyHistory(settings),
      UtxoState.genesis(
        FileHelper.getRandomTempDir,
        FileHelper.getRandomTempDir,
        settings,
        None
      ),
      List.empty[Block]
    ) {
      case ((history, state, h1, s1, blocks), i) =>
        val blockNext: Block =
          if (i > 0) {
            val boxes: Seq[AssetBox] =
              history.getBestBlock.get.payload.txs.flatMap(_.newBoxes.toList).take(30).collect {
                case a: AssetBox if a.amount > 13 => a
              }
            val txs: Vector[Transaction] =
              generatePaymentTransactions(key, boxes.toIndexedSeq, 1, 2)
            val feesTotal   = txs.map(_.fee).sum
            val supplyTotal = EncrySupplyController.supplyAt(Height @@ i, settings.constants)
            val coinbase: Transaction = TransactionFactory
              .coinbaseTransactionScratch(key.publicImage, timestamp, supplyTotal, feesTotal, Height @@ i)
            val resTxs = txs :+ coinbase
            val difficulty: Difficulty = history.getBestHeader
              .map(
                parent =>
                  history.requiredDifficultyAfter(parent) match {
                    case Right(value) => value
                    case Left(value)  => EncryApp.forceStopApplication(999, value.toString)
                }
              )
              .getOrElse(TestNetConstants.InitialDifficulty)
            val combinedStateChange: UtxoState.StateChange = combineAll(resTxs.map(UtxoState.tx2StateChange).toList)
            val newStateRoot = state.tree
              .getOperationsRootHash(
                combinedStateChange.outputsToDb.toList,
                combinedStateChange.inputsToDb.toList
              )
              .get

            val header =
              Header(
                TestNetConstants.Version,
                history.getBestHeaderId.get,
                Payload.rootHash(resTxs.map(_.id)),
                System.currentTimeMillis(),
                i,
                1,
                difficulty,
                EquihashSolution(Seq(1, 3)),
                newStateRoot
              )
            val payload = Payload(header.id, resTxs)
            val block   = Block(header, payload)
            block
          } else {
            val supplyTotal = EncrySupplyController.supplyAt(Height @@ i, settings.constants)
            val coinbase: Transaction = TransactionFactory
              .coinbaseTransactionScratch(key.publicImage, timestamp, supplyTotal, 0, Height @@ i)
            val resTxs = List(coinbase)
            val difficulty: Difficulty = history.getBestHeader
              .map(
                parent =>
                  history.requiredDifficultyAfter(parent) match {
                    case Right(value) => value
                    case Left(value)  => EncryApp.forceStopApplication(999, value.toString)
                }
              )
              .getOrElse(TestNetConstants.InitialDifficulty)
            val combinedStateChange: UtxoState.StateChange = combineAll(resTxs.map(UtxoState.tx2StateChange).toList)
            val newStateRoot = state.tree
              .getOperationsRootHash(
                combinedStateChange.outputsToDb.toList,
                combinedStateChange.inputsToDb.toList
              )
              .get

            val header =
              Header(
                0: Byte,
                Header.GenesisParentId,
                Payload.rootHash(resTxs.map(_.id)),
                System.currentTimeMillis(),
                i,
                1,
                difficulty,
                EquihashSolution(Seq(1, 3)),
                newStateRoot
              )
            val payload = Payload(header.id, resTxs)
            val block   = Block(header, payload)
            block
          }
        val h = history
          .append(blockNext.header)
          .right
          .get
          ._1
          .append(blockNext.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(blockNext)

        val s = state
          .applyModifier(blockNext.header)
          .right
          .get
          .applyModifier(blockNext)
          .right
          .get

        val his1 = h1
          .append(blockNext.header)
          .right
          .get
          ._1
          .append(blockNext.payload)
          .right
          .get
          ._1
          .reportModifierIsValid(blockNext)

        val st1 = s1
          .applyModifier(blockNext.header)
          .right
          .get
          .applyModifier(blockNext)
          .right
          .get
        (h, s, his1, st1, blocks :+ blockNext)
    }
    val (h11, s11, b11) = generateValidForHistoryAndStateBlocks(5, h, s, h.getBestBlockHeight + 1)
    val (h22, s22, b22) = generateValidForHistoryAndStateBlocks(10, h1, s1, h1.getBestBlockHeight + 1)
    ((h11, s11, (b11 ++ b).sortBy(_.header.height)), (h22, s22, b22.sortBy(_.header.height)))
  }
}
