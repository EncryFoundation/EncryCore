package encry.view.history

import encry.modifiers.InstanceFactory
import encry.network.DeliveryManagerTests.DMUtils.generateBlocks
import encry.settings.TestNetSettings
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.network.SyncInfo
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}

class ContinuationIdsTest extends WordSpecLike
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with TestNetSettings {

  "History Reader" should {

    "correctly compute continuation ids with empty history" in {
      val history: History = generateDummyHistory(testNetSettings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(testNetSettings))._2
      val syncInfo: SyncInfo = SyncInfo(blocks.map(_.header.id))

      val ids = history.continuationIds(syncInfo, 100)
      ids shouldBe Seq.empty
    }

    "correctly compute continuation ids for empty SyncInfo" in {
      val history: History = generateDummyHistory(testNetSettings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(testNetSettings))._2
      val syncInfo: SyncInfo = SyncInfo(Seq.empty)

      val updatedHistory: History = blocks.foldLeft(history) { case (hst, block) =>
        hst.append(block.header)
        hst.append(block.payload)
        hst.reportModifierIsValid(block)
      }

      val ids = updatedHistory.continuationIds(syncInfo, 100)
      ids shouldBe blocks.map(_.header.id)
    }

    "correctly compute continuation ids if our best height is higher than others best height" in {
      val history: History = generateDummyHistory(testNetSettings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(testNetSettings))._2
      val syncInfo: SyncInfo = SyncInfo(blocks.take(30).map(_.header.id))

      val updatedHistory: History = blocks.foldLeft(history) { case (hst, block) =>
        hst.append(block.header)
        hst.append(block.payload)
        hst.reportModifierIsValid(block)
      }

      val ids = updatedHistory.continuationIds(syncInfo, 100)
      ids shouldBe blocks.map(_.header.id).drop(30)
    }

    "correctly compute continuation ids if others best height is higher than our best height" in {
      val history: History = generateDummyHistory(testNetSettings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(testNetSettings))._2
      val syncInfo: SyncInfo = SyncInfo(blocks.map(_.header.id))

      val updatedHistory: History = blocks.take(30).foldLeft(history) { case (hst, block) =>
        hst.append(block.header)
        hst.append(block.payload)
        hst.reportModifierIsValid(block)
      }

      val ids = updatedHistory.continuationIds(syncInfo, 100)
      ids shouldBe blocks.map(_.header.id).take(30)
    }

  }
}
