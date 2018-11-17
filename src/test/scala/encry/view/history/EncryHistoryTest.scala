package encry.view.history

import encry.modifiers.InstanceFactory
import encry.modifiers.history.Block
import encry.settings.Constants
import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}

class EncryHistoryTest extends PropSpec with Matchers with InstanceFactory with EncryGenerator {

  property("PreGenesis height test check") {

    val history: EncryHistory = generateDummyHistory

    history.bestHeaderHeight shouldEqual Constants.Chain.PreGenesisHeight
  }

  property("Applying chain of 10 blocks to history") {

    val history: EncryHistory = generateDummyHistory
    val historyWith10Blocks: EncryHistory = (0 until 10).foldLeft(history) {
      case (prevHistory, _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header).get._1.append(block.payload).get._1
    }

    historyWith10Blocks.bestHeaderHeight shouldEqual 9
  }
}
