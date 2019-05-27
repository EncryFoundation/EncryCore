package encry.view.nvhTests

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import encry.modifiers.InstanceFactory
import encry.modifiers.history._
import encry.modifiers.mempool.Transaction
import encry.network.DeliveryManagerTests.DummyEncryAppSettingsReader
import encry.settings.{Constants, EncryAppSettings}
import encry.utils.NetworkTimeProvider
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.EncryHistory
import encry.view.state.{EncryState, UtxoState}
import org.encryfoundation.common.Algos
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class EncryNodeViewHolderTest extends WordSpecLike
  with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
  val settings: EncryAppSettings = NVHUtils.read
  val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

  override def afterAll: Unit = TestKit.shutdownActorSystem(system)

  "Nvh" should {
    "not reprocess block at height 1 if it exist" in {
      val nvhRef = NVHUtils.initDummyNvh(settings, timeProvider)
      val tmpDir: File = NVHUtils.getRandomTempDir
      val initialHistory: EncryHistory = NVHUtils.generateHistory(settings, tmpDir)
      val initState = EncryState.generateGenesisUtxoState(NVHUtils.getRandomTempDir, None, settings, None)
      val resultedHistory: (EncryHistory, Option[Block], Vector[(Block, Block)], UtxoState) =
        (0 until 10)
          .foldLeft(initialHistory, Option.empty[Block], Vector.empty[(Block, Block)], initState) {
            case ((prevHistory, prevBlock, vector, state), _) =>
              val block1: Block =
                NVHUtils.generateNextBlockValidForHistory(
                  prevHistory, 0, prevBlock,  Seq.empty[Transaction], state
                )
              val block2: Block =
                NVHUtils.generateNextBlockValidForHistory(
                  prevHistory, 1, prevBlock,  Seq.empty[Transaction], state
                )
              (prevHistory.append(block1.header).get._1.append(block1.payload).get._1.reportModifierIsValid(block1),
                Some(block1), vector :+ (block1, block2), state.applyModifier(block1).get)
          }
      resultedHistory._1.closeStorage()

      val headersFromMainChain: ModifiersFromRemote =
        ModifiersFromRemote(
          Header.modifierTypeId,
          resultedHistory._3.map(blocks => HeaderProtoSerializer.toProto(blocks._1.header).toByteArray)
        )
      val payloadsFromMainChain: ModifiersFromRemote =
        ModifiersFromRemote(
          Payload.modifierTypeId,
          resultedHistory._3.map(blocks => PayloadProtoSerializer.toProto(blocks._1.payload).toByteArray)
        )
      val firstHeaderFromFork: ModifiersFromRemote =
        ModifiersFromRemote(Header.modifierTypeId, Seq(resultedHistory._3.head._2.header.bytes))
      val firstPayloadFromFork: ModifiersFromRemote =
        ModifiersFromRemote(Payload.modifierTypeId, Seq(resultedHistory._3.head._2.payload.bytes))

      println(Algos.encode(resultedHistory._3.head._2.header.parentId))

      nvhRef ! headersFromMainChain
      nvhRef ! payloadsFromMainChain
      //nvhRef ! firstHeaderFromFork
      //nvhRef ! firstPayloadFromFork

      nvhRef.stop()
    }
  }
}
