package encry.view.state

import encry.EncryApp.args
import encry.modifiers.InstanceFactory
import encry.settings.EncryAppSettings
import encry.utils.{EncryGenerator, FileHelper}
import encry.view.history.History
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.scalatest.{Matchers, PropSpec}

//class FSUtxoStateTest extends PropSpec with Matchers with EncryGenerator with InstanceFactory {
//
//  property("fs should be correctly resolved") {
//
//    val firstSettings: EncryAppSettings = EncryAppSettings.read
//    val newNetwork = firstSettings.network.copy(networkChunkSize = 10)
//    val settings = firstSettings.copy(network = newNetwork)
//
//    val historyWith10Blocks: (History, List[Block]) = (0 until 11).foldLeft(generateDummyHistory(settings) -> List.empty[Block]) {
//      case ((prevHistory, blocks), _) =>
//        val block: Block = generateNextBlock(prevHistory, txsQty = 0)
//        prevHistory.append(block.header)
//        prevHistory.append(block.payload)
//        prevHistory.reportModifierIsValid(block) -> (blocks :+ block)
//    }
//
//    val dir = FileHelper.getRandomTempDir
//
//    val state = FSUtxoState.genesis(
//      dir,
//      None,
//      settings,
//      None
//    )
//
//    historyWith10Blocks._2.foreach(state.applyModifier)
//
//    state.resolveState(historyWith10Blocks._1)
//
//    val boxesInBlocks = historyWith10Blocks._2.flatMap(_.payload.txs.flatMap(_.newBoxes))
//
//    println(state.storage.getAll(-1).map{case (key, _) => Algos.encode(key)}.mkString(","))
//
//    val res = state.storage.getAll(-1).forall{
//      case (bxkey, bytes) =>
//        val bxRes = boxesInBlocks.exists(bx => {
//          println(s"bxRes for ${Algos.encode(bx.id)} | ${Algos.encode(bxkey)}| ${Algos.encode(bx.bytes)} | ${Algos.encode(bytes)}")
//          (bx.id sameElements bxkey) && (bx.bytes sameElements bytes)
//        })
//        bxRes
//    }
//
//    println(res)
//  }
//}
