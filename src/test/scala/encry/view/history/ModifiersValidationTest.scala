package encry.view.history

import encry.modifiers.InstanceFactory
import encry.network.DeliveryManagerTests.DMUtils.generateBlocks
import encry.settings.{EncryAppSettings, TestNetSettings}
import encry.utils.NetworkTimeProvider
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
      val newValidator = new HistoryModifiersValidator(
        newHistory.historyStorage,
        testNetSettings,
        new NetworkTimeProvider(testNetSettings.ntp)
      )
      val genesisBlock: Block = generateGenesisBlock(testNetSettings.constants.GenesisHeight)
      newValidator.testApplicable(genesisBlock.header).isRight shouldBe true
      newHistory.append(genesisBlock.header)
      val updatedHistory: History = newHistory.reportModifierIsValid(genesisBlock.header)
      newValidator.testApplicable(genesisBlock.payload).isRight shouldBe true
    }
    "reject incorrect modifiers" in {
      val blocks: List[Block] = generateBlocks(2, generateDummyHistory(testNetSettings))._2
      val newHistory: History = generateDummyHistory(testNetSettings)
      val newValidator = new HistoryModifiersValidator(
        newHistory.historyStorage,
        testNetSettings,
        new NetworkTimeProvider(testNetSettings.ntp)
      )
      blocks.take(1).foldLeft(newHistory) { case (history, block) =>
        newValidator.testApplicable(block.header).isRight shouldBe true
        history.append(block.header)
        history.reportModifierIsValid(block.header)
        newValidator.testApplicable(block.payload).isRight shouldBe true
        history.append(block.payload)
        history.reportModifierIsValid(block)
      }
      blocks.takeRight(1).foldLeft(newHistory) { case (history, block) =>
        newValidator.testApplicable(block.header).isRight shouldBe false
        history.append(block.header)
        history.reportModifierIsValid(block.header)
        newValidator.testApplicable(block.payload).isRight shouldBe true
        history.append(block.payload)
        history.reportModifierIsValid(block)
      }
    }
  }
}