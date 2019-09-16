package encry.view.history

import encry.modifiers.InstanceFactory
import encry.network.DeliveryManagerTests.DMUtils.generateBlocks
import encry.network.DeliveryManagerTests.DummyEncryAppSettingsReader
import encry.settings.EncryAppSettings
import org.encryfoundation.common.modifiers.history.Block
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}

class ModifiersValidationTest extends WordSpecLike
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  val settings: EncryAppSettings = DummyEncryAppSettingsReader.read

  "Modifiers validator" should {
    "validate genesis block" in {
      val newHistory: History = generateDummyHistory(settings)
      val genesisBlock: Block = generateGenesisBlock
      newHistory.testApplicable(genesisBlock.header).isRight shouldBe true
      newHistory.append(genesisBlock.header)
      val updatedHistory: History = newHistory.reportModifierIsValid(genesisBlock.header)
      updatedHistory.testApplicable(genesisBlock.payload).isRight shouldBe true
    }
    "reject incorrect modifiers" in {
      val blocks: List[Block] = generateBlocks(2, generateDummyHistory(settings))._2
      val newHistory: History = generateDummyHistory(settings)
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