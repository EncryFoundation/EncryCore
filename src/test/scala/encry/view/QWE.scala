//package encry.view
//
//import com.typesafe.scalalogging.StrictLogging
//import encry.modifiers.InstanceFactory
//import encry.settings.TestNetSettings
//import encry.view.history.History
//import org.encryfoundation.common.modifiers.history.Block
//import org.scalatest.{ BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike }
//
//class QWE
//    extends WordSpecLike
//    with Matchers
//    with InstanceFactory
//    with BeforeAndAfterAll
//    with OneInstancePerTest
//    with TestNetSettings
//    with StrictLogging {
//
//  "qwer" should {
//    "qr" in {
//      val (history1_10, history2_10, _) = (0 until 10).foldLeft(
//        generateDummyHistory(testNetSettings),
//        generateDummyHistory(testNetSettings),
//        List.empty[Block]
//      ) {
//        case ((prevHistory1, prevHistory2, blocks: List[Block]), _) =>
//          val block: Block = generateNextBlock(prevHistory1)
//          val a = prevHistory1
//            .append(block.header)
//            .right
//            .get
//            ._1
//            .append(block.payload)
//            .right
//            .get
//            ._1
//            .reportModifierIsValid(block)
//          val b = prevHistory2
//            .append(block.header)
//            .right
//            .get
//            ._1
//            .append(block.payload)
//            .right
//            .get
//            ._1
//            .reportModifierIsValid(block)
//          (a, b, (block +: blocks))
//      }
//      logger.info(s"\n\n\n\nStart processing  1 fork blocks\n\n\n\n\n")
//      val (history3_15norm, blocksNorm15) = (0 until 5).foldLeft(history1_10, List.empty[Block]) {
//        case ((prevHistory, blocks: List[Block]), _) =>
//          val block: Block = generateNextBlock(prevHistory)
//          prevHistory
//            .append(block.header)
//            .right
//            .get
//            ._1
//            .append(block.payload)
//            .right
//            .get
//            ._1
//            .reportModifierIsValid(block) -> (block +: blocks)
//      }
//      logger.info(s"\n\n\n\nStart processing  2  blocks\n\n\n\n\n")
//      val (h4_20, blocks4_fork) = (0 until 10).foldLeft(history2_10, List.empty[Block]) {
//        case ((prevHistory, blocks: List[Block]), _) =>
//          val block: Block = generateNextBlock(prevHistory)
//          prevHistory
//            .append(block.header)
//            .right
//            .get
//            ._1
//            .append(block.payload)
//            .right
//            .get
//            ._1
//            .reportModifierIsValid(block) -> (block +: blocks)
//      }
//
//      var tmpH = history3_15norm
//      logger.info(s"\n\n\n\nApplying fork to normal\n\n\n\n\n")
//      blocks4_fork.reverse.foreach { nextBlock =>
//        val a = tmpH.append(nextBlock.header)
//        logger.info(s"after forkapp header: ${a}")
//        tmpH = a.right.get._1
//      }
//
//      blocks4_fork.reverse.foreach { nextBlock =>
//        val a = tmpH.append(nextBlock.payload)
//        logger.info(s"after forkapp payload: ${a}")
//        tmpH = a.right.get._1
//        logger.info(s"tmpH.getBestHeader -> ${tmpH.getBestHeader}")
//        logger.info(s"tmpH.getBestBlock -> ${tmpH.getBestBlock}")
//      }
//    }
//  }
//}
