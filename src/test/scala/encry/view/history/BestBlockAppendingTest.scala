package encry.view.history

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.modifiers.history.Block
import encry.network.DeliveryManagerTests.DMUtils.generateBlocks
import encry.network.DeliveryManagerTests.DummyEncryAppSettingsReader
import encry.settings.EncryAppSettings
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class BestBlockAppendingTest extends WordSpecLike
  with BeforeAndAfterAll
  with Matchers
  with OneInstancePerTest
  with InstanceFactory with StrictLogging {

  val settings: EncryAppSettings = DummyEncryAppSettingsReader.read

  "History" should {
    "mark modifier as best after" in {
      val history: EncryHistory = generateDummyHistory(settings)

      val initialHistory1 = genForkOn(100, 1000, 25, 30, settings)

      val firstBlocks: List[Block]  = initialHistory1._1.take(30)
      val forkBlocks: List[Block]   = initialHistory1._2
      val secondBlocks: List[Block] = initialHistory1._1.takeRight(30)
      val lastBlocks: List[Block]   = initialHistory1._1.drop(30).dropRight(30)

      val firstHistoryHeaders: EncryHistory = firstBlocks.foldLeft(history) { case (h, block) =>
        h.append(block.header).get._1.reportModifierIsValid(block.header)
      }
      assert(firstHistoryHeaders.bestHeaderHeight == firstBlocks.last.header.height)
      assert(firstHistoryHeaders.bestHeaderOpt.get == firstBlocks.last.header)

      val forkHistoryHeaders: EncryHistory = forkBlocks.foldLeft(firstHistoryHeaders) { case (h, block) =>
        h.append(block.header).get._1.reportModifierIsValid(block.header)
      }
      assert(forkHistoryHeaders.bestHeaderHeight == forkBlocks.last.header.height)
      assert(forkHistoryHeaders.bestHeaderOpt.get == forkBlocks.last.header)

      val fsBlocksHistory: EncryHistory = firstBlocks.take(25).foldLeft(forkHistoryHeaders) { case (h, block) =>
        h.append(block.payload).get._1.reportModifierIsValid(block)
      }
      assert(fsBlocksHistory.bestBlockHeight == firstBlocks.take(25).last.header.height)
      assert(fsBlocksHistory.bestBlockOpt.get == firstBlocks.take(25).last)

      val forkBlocksHistory: EncryHistory = forkBlocks.foldLeft(fsBlocksHistory) { case (h, block) =>
        h.append(block.payload).get._1.reportModifierIsValid(block)
      }
      assert(forkBlocksHistory.bestBlockHeight == forkBlocks.last.header.height)
      assert(forkBlocksHistory.bestBlockOpt.get == forkBlocks.last)

      val fsBlocksHistory1: EncryHistory = firstBlocks.takeRight(5).foldLeft(forkBlocksHistory) { case (h, block) =>
        h.append(block.payload).get._1
      }
      assert(fsBlocksHistory1.bestBlockHeight == forkBlocks.last.header.height)
      assert(fsBlocksHistory1.bestBlockOpt.get == forkBlocks.last)

      val fsBlocksHistory1R1: EncryHistory = initialHistory1._1.takeRight(70).foldLeft(fsBlocksHistory1) { case (h, block) =>
        h.append(block.header).get._1.reportModifierIsValid(block.header)
      }
      val fsBlocksHistory1R2: EncryHistory = initialHistory1._1.takeRight(70).foldLeft(fsBlocksHistory1R1) { case (h, block) =>
        h.append(block.payload).get._1.reportModifierIsValid(block)
      }
      assert(fsBlocksHistory1R2.bestBlockHeight == initialHistory1._1.last.header.height)
      assert(fsBlocksHistory1R2.bestBlockOpt.get == initialHistory1._1.last)
    }
  }
}