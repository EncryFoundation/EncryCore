package encry.view.history

import encry.modifiers.InstanceFactory
import encry.network.DeliveryManagerTests.DMUtils.generateBlocks
import encry.settings.{EncryAppSettings, MainTestSettings}
import org.encryfoundation.common.modifiers.history.Block
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}

class ModifiersValidationTest extends WordSpecLike
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with MainTestSettings {

  "Modifiers validator" should {
    "validate genesis block" in {
      val newHistory: History = generateDummyHistory(mainTestSettings)
      val genesisBlock: Block = generateGenesisBlock(mainTestSettings.constants.GenesisHeight)
      newHistory.testApplicable(genesisBlock.header).isRight shouldBe true
      val updatedHistory: History =
        newHistory.append(genesisBlock.header).right.get._1.reportModifierIsValid(genesisBlock.header)
      updatedHistory.testApplicable(genesisBlock.payload).isRight shouldBe true
    }
    "reject incorrect modifiers" in {
      val blocks: List[Block] = generateBlocks(2, generateDummyHistory(mainTestSettings))._2
      val newHistory: History = generateDummyHistory(mainTestSettings)
      blocks.take(1).foldLeft(newHistory) { case (history, block) =>
        history.testApplicable(block.header).isRight shouldBe true
        history.append(block.header).right.get._1.reportModifierIsValid(block.header)
        history.testApplicable(block.payload).isRight shouldBe true
        history.append(block.payload).right.get._1.reportModifierIsValid(block)
      }
      blocks.takeRight(1).foldLeft(newHistory) { case (history, block) =>
        history.testApplicable(block.header).isRight shouldBe false
        history.append(block.header).right.get._1.reportModifierIsValid(block.header)
        history.testApplicable(block.payload).isRight shouldBe true
        history.append(block.payload).right.get._1.reportModifierIsValid(block)
      }
    }
  }
}