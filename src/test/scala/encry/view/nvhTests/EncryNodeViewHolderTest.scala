package encry.view.nvhTests

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.modifiers.history._
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.network.DeliveryManagerTests.DummyEncryAppSettingsReader
import encry.settings.{Constants, EncryAppSettings}
import encry.storage.VersionalStorage
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.NetworkTimeProvider
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.EncryHistory
import encry.view.state.{BoxHolder, EncryState, UtxoState}
import org.encryfoundation.common.Algos
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

import scala.concurrent.Await

class EncryNodeViewHolderTest extends WordSpecLike
  with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with StrictLogging {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
  val settings: EncryAppSettings = NVHUtils.read
  val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

  override def afterAll: Unit = TestKit.shutdownActorSystem(system)

  "Nvh" should {
    "not reprocess block at height 1 if it exist" in {
      val nvhRef = NVHUtils.initDummyNvh(settings, timeProvider)

      def initialBoxes: IndexedSeq[AssetBox] = IndexedSeq(AssetBox(EncryProposition.open, -9, 0))
      val boxesHolder: BoxHolder = BoxHolder(initialBoxes)
      val initState: UtxoState = NVHUtils.utxoFromBoxHolder(boxesHolder, NVHUtils.getRandomTempDir, None, settings, VersionalStorage.IODB)
      val genesisBlock = NVHUtils.generateGenesisBlockValidForState(initState)
      val initialHistory: EncryHistory =
        NVHUtils.generateHistory(settings, NVHUtils.getRandomTempDir)
          .append(genesisBlock.header)
          .get._1
          .append(genesisBlock.payload)
          .get._1
      val stateAfterGenesisBlock = initState.applyModifier(genesisBlock).get
      val resultedHistory: (EncryHistory, Option[Block], List[Block], UtxoState, VersionTag) =
        (0 to 8)
          .foldLeft(initialHistory, Some(genesisBlock), List(genesisBlock), stateAfterGenesisBlock, VersionTag @@ Array.emptyByteArray) {
            case ((prevHistory, prevBlock, vector, prevState, _), i) =>
              val block1: Block =
                NVHUtils.generateNextBlockValidForHistory(
                  prevHistory, 0, prevBlock,  Seq.empty[Transaction], prevState, i == 8
                )
              val prevStateVersion = prevState.version
              val newState = prevState.applyModifier(block1).get
              (prevHistory.append(block1.header).get._1.append(block1.payload).get._1,
                Some(block1), vector :+ block1, newState, prevStateVersion)
          }

      val stateAfterRollBack = resultedHistory._4.rollbackTo(resultedHistory._5).get

      println(resultedHistory._3.init.lastOption.map(_.header))

      val block1: Block =
        NVHUtils.generateNextBlockValidForHistory(
          resultedHistory._1, 0, resultedHistory._3.init.lastOption,  Seq.empty[Transaction], stateAfterRollBack, false
        )

      println(block1.header)

      val stateAfter1Block = stateAfterRollBack.applyModifier(block1).get

      val historyAfter1Block = resultedHistory._1.append(block1.header).get._1.append(block1.payload).get._1

      val block2: Block =
        NVHUtils.generateNextBlockValidForHistory(
          resultedHistory._1, BigInt(100000), Some(block1),  Seq.empty[Transaction], stateAfterRollBack, true
        )

      println(block2.header)

      val stateAfter2Block = stateAfter1Block.applyModifier(block2).get

      val historyAfter2Block = historyAfter1Block.append(block2.header).get._1.append(block2.payload).get._1

      resultedHistory._1.closeStorage()

      val headersFromMainChain: ModifiersFromRemote =
        ModifiersFromRemote(
          Header.modifierTypeId,
          resultedHistory._3.map(blocks => HeaderProtoSerializer.toProto(blocks.header).toByteArray)
        )
      val payloadsFromMainChain: ModifiersFromRemote =
        ModifiersFromRemote(
          Payload.modifierTypeId,
          resultedHistory._3.map(blocks => PayloadProtoSerializer.toProto(blocks.payload).toByteArray)
        )
      val headersFromFromFork: ModifiersFromRemote =
        ModifiersFromRemote(
          Header.modifierTypeId,
          List(block1).map(blocks => HeaderProtoSerializer.toProto(blocks.header).toByteArray)
        )
      val headersFromFromFork2: ModifiersFromRemote =
        ModifiersFromRemote(
          Header.modifierTypeId,
          List(block2).map(blocks => HeaderProtoSerializer.toProto(blocks.header).toByteArray)
        )
      val payloadsFromFromFork: ModifiersFromRemote =
        ModifiersFromRemote(
          Payload.modifierTypeId,
          List(block1, block2).map(blocks => PayloadProtoSerializer.toProto(blocks.payload).toByteArray)
        )

      nvhRef ! headersFromMainChain
      nvhRef ! payloadsFromMainChain
      nvhRef ! headersFromFromFork
      Thread.sleep(5000)
      nvhRef ! headersFromFromFork2
      nvhRef ! payloadsFromFromFork

      Thread.sleep(5000)

      nvhRef.stop()
    }
  }
}
