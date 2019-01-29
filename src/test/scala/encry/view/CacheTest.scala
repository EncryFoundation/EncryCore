package encry.view

import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.modifiers.{EncryPersistentModifier, InstanceFactory}
import encry.modifiers.history.{Block, Header, Payload}
import encry.settings.{Constants, EncryAppSettings}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.utils.EncryGenerator
import encry.view.history.EncryHistory
import org.scalatest.{Matchers, PropSpec}
import encry.view.ModifiersCache

import scala.collection.mutable
import scala.util.{Random => Scarand}

class CacheTest extends PropSpec with Matchers with EncryGenerator with InstanceFactory {

  val settings: EncryAppSettings = EncryAppSettings.read

  property("Applying 10 blocks to history") {

    val historyWith30Blocks: EncryHistory = (0 until 30).foldLeft(generateDummyHistory(settings)) {
      case (prevHistory, _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
    }

    val next60Blocks: (List[Block], Block) = (30 until 90).foldLeft(List[Block](), historyWith30Blocks.bestBlockOpt.get) {
      case ((blocks, prevBestBlock), _) =>
        val nextBlock: Block = generateNextBlock(prevBestBlock)
        (nextBlock :: blocks, nextBlock)
    }

    next60Blocks._1.foreach(h => ModifiersCache.put(key(h.id), h, historyWith30Blocks))

    val historyLast: EncryHistory = computeApplications(historyWith30Blocks)

    historyLast.bestBlockHeight shouldBe 89
  }

  def generateNextBlock(prevBlock: Block): Block = {
    val txs = genValidPaymentTxs(Scarand.nextInt(10)) ++ Seq(coinbaseTransaction)
    val header = genHeader.copy(
      parentId = prevBlock.id,
      height = prevBlock.header.height + 1,
      transactionsRoot = Payload.rootHash(txs.map(_.id))
    )
    Block(header, Payload(header.id, txs), None)
  }

  def key(id: ModifierId): mutable.WrappedArray.ofByte = new mutable.WrappedArray.ofByte(id)

  def computeApplications(history: EncryHistory): EncryHistory = {
    val mods: List[EncryPersistentModifier] = ModifiersCache.popCandidate(history)
    if (mods.nonEmpty) {
      mods.size shouldBe 1
      val historyNew = mods.foldLeft(history) { case (historyInner, mod) => historyInner.append(mod).get._1 }
      computeApplications(historyNew)
    }
    else history
  }
}