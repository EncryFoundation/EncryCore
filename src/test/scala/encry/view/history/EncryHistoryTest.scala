package encry.view.history

import encry.modifiers.InstanceFactory
import encry.modifiers.history.Block
import encry.settings.Constants
import encry.utils.EncryGenerator
import org.encryfoundation.common.Algos
import org.scalatest.{Matchers, PropSpec}

class EncryHistoryTest extends PropSpec with Matchers with InstanceFactory with EncryGenerator {

  property("PreGenesis height test check") {

    val history: EncryHistory = generateDummyHistory

    history.bestHeaderHeight shouldEqual Constants.Chain.PreGenesisHeight
  }

  property("Applying 10 blocks to history") {

    val historyWith10Blocks: EncryHistory = (0 until 10).foldLeft(generateDummyHistory) {
      case (prevHistory, _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
    }

    historyWith10Blocks.bestHeaderHeight shouldEqual 9
    historyWith10Blocks.bestBlockHeight shouldEqual 9
  }

  property("Fork resolving with different different cumulative sum of blocks"){

    val historyWith35Blocks: EncryHistory = (0 until 35).foldLeft(generateDummyHistory) {
      case (prevHistory, _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header).get._1.reportModifierIsValid(block.payload)
    }

    val historyWith40Blocks: EncryHistory = (0 until 5).foldLeft(historyWith35Blocks) {
      case (prevHistory, _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header).get._1.reportModifierIsValid(block.payload)
    }

    val forkBlocks: Seq[Block] = (0 until 6).foldLeft(historyWith35Blocks, Seq[Block]()) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header).get._1.reportModifierIsValid(block.payload) -> (blocks :+ block)
    }._2

    val historyAfterForkBlocksApplying = forkBlocks.foldLeft(historyWith40Blocks) {
      case (prevHistory, blockToApply) =>
        prevHistory.append(blockToApply.header).get._1.reportModifierIsValid(blockToApply.payload)
    }
  }


}
