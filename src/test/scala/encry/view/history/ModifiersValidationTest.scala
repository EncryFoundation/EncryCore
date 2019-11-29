package encry.view.history

import encry.modifiers.InstanceFactory
import encry.network.DeliveryManagerTests.DMUtils.generateBlocks
import encry.network.DownloadedModifiersValidator.ModifierWithBytes
import encry.settings.{EncryAppSettings, TestNetSettings}
import org.encryfoundation.common.modifiers.history.Block
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}

class ModifiersValidationTest extends WordSpecLike
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with TestNetSettings {

  "Modifiers validator" should {
    "validate genesis block" in {
      val newHistory: History = generateDummyHistory(testNetSettings)
      val genesisBlock: Block = generateGenesisBlock(testNetSettings.constants.GenesisHeight)
      newHistory.testApplicable(genesisBlock.header).isRight shouldBe true
      newHistory.append(ModifierWithBytes(genesisBlock.header))
      val updatedHistory: History = newHistory.reportModifierIsValid(genesisBlock.header)
      updatedHistory.testApplicable(genesisBlock.payload).isRight shouldBe true
    }
    "reject incorrect modifiers" in {
      val blocks: List[Block] = generateBlocks(2, generateDummyHistory(testNetSettings))._2
      val newHistory: History = generateDummyHistory(testNetSettings)
      blocks.take(1).foldLeft(newHistory) { case (history, block) =>
        history.testApplicable(block.header).isRight shouldBe true
        history.append(ModifierWithBytes(block.header))
        history.reportModifierIsValid(block.header)
        history.testApplicable(block.payload).isRight shouldBe true
        history.append(ModifierWithBytes(block.payload))
        history.reportModifierIsValid(block)
      }
      blocks.takeRight(1).foldLeft(newHistory) { case (history, block) =>
        history.testApplicable(block.header).isRight shouldBe false
        history.append(ModifierWithBytes(block.header))
        history.reportModifierIsValid(block.header)
        history.testApplicable(block.payload).isRight shouldBe true
        history.append(ModifierWithBytes(block.payload))
        history.reportModifierIsValid(block)
      }
    }
  }
}