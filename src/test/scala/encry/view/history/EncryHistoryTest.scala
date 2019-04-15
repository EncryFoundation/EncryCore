package encry.view.history

import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header}
import encry.network.AuxiliaryHistoryHolder.AuxHistoryChanged
import encry.settings.{Constants, EncryAppSettings}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.utils.EncryGenerator
import org.encryfoundation.common.Algos
import org.scalatest.{Matchers, PropSpec}

class EncryHistoryTest extends PropSpec with Matchers with InstanceFactory with EncryGenerator {

  property("PreGenesis height test check.") {

//    val history: EncryHistory = generateDummyHistory

//    history.bestHeaderHeight shouldEqual Constants.Chain.PreGenesisHeight
  }
//
  property("Applying 10 blocks to history") {

    val settings = EncryAppSettings.read

    val historyWith10Blocks: EncryHistory = (0 until 150).foldLeft(generateDummyHistory(settings)) {
      case (prevHistory, _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
    }

    val blocks = (0 until 10).foldLeft(generateDummyHistory(settings), List.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block) -> (blocks :+ block)
    }._2

    historyWith10Blocks.append(blocks.head.header).get._1.append(blocks.head.payload).get._1.reportModifierIsValid(blocks.head)

    historyWith10Blocks.bestHeaderHeight shouldEqual 9
    historyWith10Blocks.bestBlockHeight shouldEqual 9
  }
//
//  property("Fork with different difficult in fork block"){
//
//    val historyWith35Blocks: EncryHistory = (0 until 35).foldLeft(generateDummyHistory) {
//      case (prevHistory, _) =>
//        val block: Block = generateNextBlock(prevHistory)
//        prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
//    }
//
//    val (historyWith40Blocks: EncryHistory, fork: Seq[Block]) = (0 until 5).foldLeft(historyWith35Blocks, Seq[Block]()) {
//      case ((prevHistory, forkBlocks), _) =>
//        val mainBlock: Block = generateNextBlock(prevHistory)
//        val forkBlock: Block = generateNextBlock(prevHistory, prevId = forkBlocks.lastOption.map(_.id))
//        prevHistory.append(mainBlock.header).get._1.append(mainBlock.payload).get._1.reportModifierIsValid(mainBlock) ->
//          (forkBlocks :+ forkBlock)
//    }
//
//    val historyWithFork: EncryHistory = fork.foldLeft(historyWith40Blocks) {
//      case (prevHistory, blockToApply) =>
//        prevHistory.append(blockToApply.header).get._1.append(blockToApply.payload).get._1
//    }
//
//    val historyAfterSimpleFork: EncryHistory = (0 until 5).foldLeft(historyWithFork) {
//      case (prevHistory , i) =>
//        val mainBlock: Block =
//          if (i == 0) generateNextBlock(prevHistory, prevId = Some(fork.last.id))
//          else generateNextBlock(prevHistory)
//        prevHistory.append(mainBlock.header).get._1.append(mainBlock.payload).get._1.reportModifierIsValid(mainBlock)
//    }
//
//    historyAfterSimpleFork.headerIdsAtHeight(34).length shouldEqual 1
//    historyAfterSimpleFork.headerIdsAtHeight(35).length shouldEqual 2 //two headers at height
//    historyAfterSimpleFork.headerIdsAtHeight(36).length shouldEqual 2
//    historyAfterSimpleFork.headerIdsAtHeight(37).length shouldEqual 2
//    historyAfterSimpleFork.headerIdsAtHeight(38).length shouldEqual 2
//    historyAfterSimpleFork.headerIdsAtHeight(39).length shouldEqual 2
//    historyAfterSimpleFork.headerIdsAtHeight(40).length shouldEqual 1
//    historyAfterSimpleFork.headerIdsAtHeight(40)
//      .map(historyAfterSimpleFork.typedModifierById[Header])
//      .head
//      .get
//      .parentId shouldEqual fork.last.header.id
//  }
//
//  property("Fork on non-best-height, but with bigger commulative sum"){
//
//    val historyWith35Blocks: EncryHistory = (0 until 35).foldLeft(generateDummyHistory) {
//      case (prevHistory, _) =>
//        val block: Block = generateNextBlock(prevHistory)
//        val startTime = System.currentTimeMillis()
//        val history = prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
//        println(s"Apply block on height: ${block.header.height}. ApplyTime: ${System.currentTimeMillis() - startTime}")
//        history
//    }
//
//    val (historyWith40Blocks: EncryHistory, fork: Seq[Block]) = (0 until 5).foldLeft(historyWith35Blocks, Seq[Block]()) {
//      case ((prevHistory, forkBlocks), _) =>
//        val mainBlock: Block = generateNextBlock(prevHistory)
//        val forkBlock: Block = generateNextBlock(prevHistory, 100000000, prevId = forkBlocks.lastOption.map(_.id))
//        prevHistory.append(mainBlock.header).get._1.append(mainBlock.payload).get._1.reportModifierIsValid(mainBlock) ->
//          (forkBlocks :+ forkBlock)
//    }
//
//    val blocksHeightWithoutForks: Int = historyWith40Blocks.bestBlockHeight
//    val headersHeightWithoutForks: Int = historyWith40Blocks.bestHeaderHeight
//    val bestHeaderIdWithoutForks: ModifierId = historyWith40Blocks.bestHeaderOpt.get.id
//    val bestBlockIdWithoutForks: ModifierId = historyWith40Blocks.bestBlockIdOpt.get
//
//    val historyWithFork: EncryHistory = fork.take(3).foldLeft(historyWith40Blocks) {
//      case (prevHistory, blockToApply) =>
//        prevHistory.append(blockToApply.header).get._1.append(blockToApply.payload).get._1
//    }
//
////    historyWithFork.headerIdsAtHeight(34).length shouldEqual 1
////    historyWithFork.headerIdsAtHeight(35).length shouldEqual 2 //two headers at height
////    historyWithFork.headerIdsAtHeight(36).length shouldEqual 2
////    historyWithFork.headerIdsAtHeight(37).length shouldEqual 2
////    historyWithFork.headerIdsAtHeight(38).length shouldEqual 1
//    blocksHeightWithoutForks shouldEqual historyWithFork.bestBlockHeight
//    headersHeightWithoutForks shouldEqual historyWithFork.bestHeaderHeight
//    historyWithFork.bestBlockHeight shouldEqual historyWithFork.bestHeaderHeight
//    bestHeaderIdWithoutForks shouldEqual historyWithFork.bestHeaderOpt.get.id
//    bestBlockIdWithoutForks shouldEqual historyWithFork.bestBlockIdOpt.get
//  }
}
