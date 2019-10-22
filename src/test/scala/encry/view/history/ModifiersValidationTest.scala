package encry.view.history

import encry.network.DeliveryManagerTests.DeliveryManagerUtils.generateBlocks
import encry.settings.TestNetSettings
import encry.utils.ChainGenerator.genGenesisBlock
import encry.utils.HistoryGenerator.dummyHistory
import encry.utils.Keys
import org.encryfoundation.common.modifiers.history.Block
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}

class ModifiersValidationTest extends WordSpecLike
  with Matchers
  with OneInstancePerTest
  with Keys
  with TestNetSettings {

  "Modifiers validator" should {
    "validate genesis block" in {
      val newHistory: History = dummyHistory(testNetSettings)
      val genesisBlock: Block =  genGenesisBlock(privKey.publicImage, testNetSettings.constants.InitialEmissionAmount,
        testNetSettings.constants.InitialDifficulty, testNetSettings.constants.GenesisHeight)
      newHistory.testApplicable(genesisBlock.header).isRight shouldBe true
      newHistory.append(genesisBlock.header)
      val updatedHistory: History = newHistory.reportModifierIsValid(genesisBlock.header)
      updatedHistory.testApplicable(genesisBlock.payload).isRight shouldBe true
    }
    "reject incorrect modifiers" in {
      val blocks: List[Block] = generateBlocks(2, dummyHistory(testNetSettings))._2
      val newHistory: History = dummyHistory(testNetSettings)
      blocks.take(1).foldLeft(newHistory) { case (history, block) =>
        history.testApplicable(block.header).isRight shouldBe true
        history.append(block.header)
        history.reportModifierIsValid(block.header)
        history.testApplicable(block.payload).isRight shouldBe true
        history.append(block.payload)
        history.reportModifierIsValid(block)
      }
      blocks.takeRight(1).foldLeft(newHistory) { case (history, block) =>
        history.testApplicable(block.header).isRight shouldBe false
        history.append(block.header)
        history.reportModifierIsValid(block.header)
        history.testApplicable(block.payload).isRight shouldBe true
        history.append(block.payload)
        history.reportModifierIsValid(block)
      }
    }
  }
}