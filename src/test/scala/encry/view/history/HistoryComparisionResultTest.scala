package encry.view.history

import encry.consensus.History._
import encry.modifiers.InstanceFactory
import encry.modifiers.history.Block
import encry.network.DeliveryManagerTests.DMUtils.generateBlocks
import encry.network.DeliveryManagerTests.DummyEncryAppSettingsReader
import encry.settings.EncryAppSettings
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}

class HistoryComparisionResultTest extends WordSpecLike
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  val settings: EncryAppSettings = DummyEncryAppSettingsReader.read

  "History Reader" should {
    "mark history as Equal where our best header is the same as other history best header" in {
      val history: EncryHistory = generateDummyHistory(settings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(settings))._2
      val syncInfo: EncrySyncInfo = EncrySyncInfo(blocks.map(_.header.id))

      val updatedHistory: EncryHistory = blocks.foldLeft(history) { case (hst, block) =>
        hst.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
      }

      val comparisonResult = updatedHistory.compare(syncInfo)
      assert(comparisonResult == Equal)
    }

    "mark history as Older where our best header is in other history best chain, but not at the last position" in {
      val history: EncryHistory = generateDummyHistory(settings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(settings))._2
      val syncInfo: EncrySyncInfo = EncrySyncInfo(blocks.map(_.header.id))

      val updatedHistory: EncryHistory = blocks.take(50).foldLeft(history) { case (hst, block) =>
        hst.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
      }

      val comparisonResult = updatedHistory.compare(syncInfo)
      assert(comparisonResult == Older)
    }

    "mark history as Younger when comparing history is empty" in {
      val history: EncryHistory = generateDummyHistory(settings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(settings))._2
      val syncInfo: EncrySyncInfo = EncrySyncInfo(Seq.empty)

      val updatedHistory: EncryHistory = blocks.foldLeft(history) { case (hst, block) =>
        hst.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
      }

      val comparisonResult = updatedHistory.compare(syncInfo)
      assert(comparisonResult == Younger)
    }

    "mark history as Younger when our history contains all other history but other history " +
      "doesn't contain our last 70 headers" in {
      val history: EncryHistory = generateDummyHistory(settings)
      val blocks: List[Block] = generateBlocks(100, generateDummyHistory(settings))._2
      val syncInfo: EncrySyncInfo = EncrySyncInfo(blocks.take(30).map(_.header.id))

      val updatedHistory: EncryHistory = blocks.foldLeft(history) { case (hst, block) =>
        hst.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
      }

      val comparisonResult = updatedHistory.compare(syncInfo)
      assert(comparisonResult == Younger)
    }

    "mark history as Fork when we have same point in histories" in {
      val history: EncryHistory = generateDummyHistory(settings)

      val fork = genForkOn(100, 1000, 25, 30, settings)

      val syncInfo: EncrySyncInfo = EncrySyncInfo(
        fork._1.take(25).map(_.header.id) ++: fork._2.map(_.header.id)
      )

      val updatedHistory: EncryHistory = fork._1.take(30).foldLeft(history) { case (hst, block) =>
        hst.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
      }

      val comparisonResult = updatedHistory.compare(syncInfo)
      assert(comparisonResult == Fork)
    }

    "mark history as Equal where both nodes do not keep any blocks" in {
      val history: EncryHistory = generateDummyHistory(settings)
      val syncInfo: EncrySyncInfo = EncrySyncInfo(Seq.empty)

      val comparisonResult = history.compare(syncInfo)
      assert(comparisonResult == Equal)
    }

    "mark history as Older " in {
      val history: EncryHistory = generateDummyHistory(settings)
      val syncInfo: EncrySyncInfo = EncrySyncInfo(
        generateBlocks(30, generateDummyHistory(settings))._2.map(_.header.id))

      val comparisonResult = history.compare(syncInfo)
      assert(comparisonResult == Older)
    }
  }
}