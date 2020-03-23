package encry.nvg

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus
import encry.consensus.HistoryConsensus.Equal
import encry.modifiers.InstanceFactory
import encry.modifiers.history.HeaderChain
import encry.nvg.NVHState.StateAction.ApplyModifier
import encry.utils.FileHelper
import encry.view.history.HistoryReader
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class NVHStateTest
  extends TestKit(ActorSystem("Tested-Akka-System"))
  with WordSpecLike
  with Matchers
  with BeforeAndAfterAll
  with InstanceFactory
  with OneInstancePerTest
  with StrictLogging {

  "Nvh state" should {
    "correctly init genesis state" in {
      val stateDir = FileHelper.getRandomTempDir
      val actor = TestActorRef[NVHState](NVHState.genesisProps(settings.copy(directory = stateDir.getAbsolutePath), None))
    }
    "correctly recover state" in {

      val ((_, _, blocks), _) = PipelinesTests.genForOn(3)

      val historyReader = new HistoryReader {

        override def getBestHeaderHeight: Int = blocks.last.header.height

        override def getBestBlockHeight: Int = blocks.last.header.height

        override def getBestHeaderAtHeight(h: Int): Option[Header] = Some(blocks(h).header)

        override def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId] = Seq.empty

        override def compare(si: SyncInfo): HistoryConsensus.HistoryComparisonResult = Equal

        override def getHeaderById(id: ModifierId): Option[Header] = blocks.find(_.id sameElements id).map(_.header)

        override def getChainToHeader(fromHeaderOpt: Option[Header],
                                      toHeader: Header): (Option[ModifierId], HeaderChain) =
          fromHeaderOpt.map(header =>
            Some(header.id) -> HeaderChain(blocks.dropWhile(block => !(block.id sameElements header.id)).map(_.header))
          ).getOrElse(None -> HeaderChain.empty)

        override def getBlockByHeaderId(id: ModifierId): Option[Block] = blocks.find(_.id sameElements id)

        override def getBlockByHeader(header: Header): Option[Block] = blocks.find(_.id sameElements header.id)

        override var isFullChainSynced: Boolean = true

        override def isModifierDefined(id: ModifierId): Boolean = blocks.exists(_.id sameElements id)

        override def headerIdsAtHeight(height: Int): List[ModifierId] = List(blocks(height).id)

        override def modifierBytesById(id: ModifierId): Option[Array[Byte]] = blocks.find(_.id sameElements id).map(_.bytes)

        override def payloadsIdsToDownload(howMany: Int): Seq[ModifierId] = Seq.empty

        override def lastHeaders(count: Int): HeaderChain = HeaderChain.empty

        override def syncInfo: SyncInfo = SyncInfo(List.empty)

        override def isFastSyncInProcess: Boolean = false

        override def getBestHeader: Option[Header] = Some(blocks.last.header)

        override def getBestBlock: Option[Block] = Some(blocks.last)

      }

      val stateDir = FileHelper.getRandomTempDir
      val emptyHistoryReader = HistoryReader.empty
      val parent = TestProbe()
      val actor = TestActorRef[NVHState](
        NVHState.genesisProps(settings.copy(directory = stateDir.getAbsolutePath), None), parent.ref
      )
      blocks.sortBy(_.header.height).foreach { block =>
        actor ! ApplyModifier(block.header, saveRootNodesFlag = true, isFullChainSynced = true)
        actor ! ApplyModifier(block, saveRootNodesFlag = true, isFullChainSynced = true)
      }
      Thread.sleep(5000)
      actor.stop()
      val recreatedActor = TestActorRef[NVHState](NVHState.restoreConsistentStateProps(
        settings.copy(directory = stateDir.getAbsolutePath),
        historyReader,
        Some(TestProbe().ref)
      ).get)
      recreatedActor.underlyingActor.state.height shouldEqual blocks.last.header.height
      recreatedActor.underlyingActor.state.tree.avlStorage.currentVersion shouldEqual blocks.last.id
    }
  }
}
