package encry.view.history

import encry.consensus.HistoryConsensus._
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManagerTests.DMUtils.generateBlocks
import encry.settings.{EncryAppSettings, TestNetSettings}
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.network.SyncInfo
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}

class HistoryComparisionResultTest extends WordSpecLike
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with TestNetSettings {

  "History Reader" should {
    "mark history as Equal where our best header is the same as other history best header" in {
      val history: History = generateDummyHistory(testNetSettings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(testNetSettings))._2
      val syncInfo: SyncInfo = SyncInfo(blocks.map(_.header.id))

      val updatedHistory: History = blocks.foldLeft(history) { case (hst, block) =>
        hst.append(block.header)
        hst.calculateNewSyncInfo()
        hst.append(block.payload)
        hst.reportModifierIsValid(block)
      }

      val comparisonResult = updatedHistory.compare(syncInfo)
      assert(comparisonResult == Equal)
    }

    "mark history as Older where our best header is in other history best chain, but not at the last position" in {
      val history: History = generateDummyHistory(testNetSettings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(testNetSettings))._2
      val syncInfo: SyncInfo = SyncInfo(blocks.map(_.header.id))

      val updatedHistory: History = blocks.take(50).foldLeft(history) { case (hst, block) =>
        hst.append(block.header)
        hst.calculateNewSyncInfo()
        hst.append(block.payload)
        hst.reportModifierIsValid(block)
      }

      val comparisonResult = updatedHistory.compare(syncInfo)
      assert(comparisonResult == Older)
    }

    "mark history as Younger when comparing history is empty" in {
      val history: History = generateDummyHistory(testNetSettings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(testNetSettings))._2
      val syncInfo: SyncInfo = SyncInfo(Seq.empty)

      val updatedHistory: History = blocks.foldLeft(history) { case (hst, block) =>
        hst.append(block.header)
        hst.calculateNewSyncInfo()
        hst.append(block.payload)
        hst.reportModifierIsValid(block)
      }

      val comparisonResult = updatedHistory.compare(syncInfo)
      assert(comparisonResult == Younger)
    }

    "mark history as Younger when our history contains all other history but other history " +
      "doesn't contain our last 70 headers" in {
      val history: History = generateDummyHistory(testNetSettings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(testNetSettings))._2
      val syncInfo: SyncInfo = SyncInfo(blocks.take(30).map(_.header.id))

      val updatedHistory: History = blocks.foldLeft(history) { case (hst, block) =>
        hst.append(block.header)
        hst.calculateNewSyncInfo()
        hst.append(block.payload)
        hst.reportModifierIsValid(block)
      }

      val comparisonResult = updatedHistory.compare(syncInfo)
      assert(comparisonResult == Younger)
    }

    "mark history as Fork when we have same point in histories" in {
      val history: History = generateDummyHistory(testNetSettings)

      val fork = genForkOn(100, 1000, 25, 30, testNetSettings)

      val syncInfo: SyncInfo = SyncInfo(
        fork._1.take(25).map(_.header.id) ++: fork._2.map(_.header.id)
      )

      val updatedHistory: History = fork._1.take(30).foldLeft(history) { case (hst, block) =>
        hst.append(block.header)
        hst.calculateNewSyncInfo()
        hst.append(block.payload)
        hst.reportModifierIsValid(block)
      }

      val comparisonResult = updatedHistory.compare(syncInfo)
      assert(comparisonResult == Fork)
    }

    "mark history as Equal where both nodes do not keep any blocks" in {
      val history: History = generateDummyHistory(testNetSettings)
      val syncInfo: SyncInfo = SyncInfo(Seq.empty)

      val comparisonResult = history.compare(syncInfo)
      assert(comparisonResult == Equal)
    }

    "mark history as Older " in {
      val history: History = generateDummyHistory(testNetSettings)
      val syncInfo: SyncInfo = SyncInfo(
        generateBlocks(30, generateDummyHistory(testNetSettings))._2.map(_.header.id))

      val comparisonResult = history.compare(syncInfo)
      assert(comparisonResult == Older)
    }
  }
}