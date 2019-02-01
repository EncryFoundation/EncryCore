package encry.view.history

import encry.consensus.History
import encry.modifiers.{EncryPersistentModifier, InstanceFactory}
import encry.modifiers.history.{Block, Header}
import encry.settings.EncryAppSettings
import encry.utils.{EncryGenerator, NetworkTimeProvider}
import io.iohk.iodb.ByteArrayWrapper
import org.scalatest.{Matchers, PropSpec}

class BlockProcessorTest extends PropSpec with Matchers with InstanceFactory with EncryGenerator {

  val settings: EncryAppSettings = EncryAppSettings.read

  property("PreGenesis height test check.") {

    lazy val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

    val history: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)

    val bestBlock: Block = history.bestBlockOpt.get
    println(bestBlock.header.height)

    //    println(bestBlock.header.height)
    //
    //    val lastBlocks: IndexedSeq[Block] = history.lastHeaders(35).headers.map(x => history.getBlock(x))
    //      .collect { case Some(v) => v }.sortBy(x => x.header.height)
    //
    //    println(lastBlocks.exists(x => x.id sameElements bestBlock.id))
    //
    //    val tryToUpdateHistory = lastBlocks.foldLeft(history) { case (history1, block2) =>
    //      println(s"nextBlock -> ${block2.header.height}")
    //      val newH = history1.append(block2.header)
    //      newH match {
    //        case Success(v) =>
    //          println(s"Appended header at height ${block2.header.height}")
    //          v._1.append(block2.payload) match {
    //            case Success(f) =>
    //              println(s"Appended payload at height ${block2.header.height}. NEw best block is: ${f._1.bestBlockOpt.get}")
    //              f._1
    //            case Failure(ex) =>
    //              println(s"$ex ")
    //              history1
    //          }
    //        case Failure(ex) =>
    //          println(s"$ex")
    //          history1
    //      }
    //    }

    val nextHeaderId = history.headerIdsAtHeight(bestBlock.header.height + 1).head
    val nextHeader: Header = history.modifierById(nextHeaderId).get.asInstanceOf[Header]
    val nextBlock: Block = history.getBlock(nextHeader).get

    println(nextBlock.header.height)

    val newHist: Unit = history.historyStorage.store.rollback(ByteArrayWrapper(bestBlock.id))

    val newHistoryR: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)

    val lastBlocks: Block = newHistoryR.lastHeaders(50).headers.map(x => history.getBlock(x))
      .collect { case Some(v) => v }.minBy(x => x.header.height)

    println(lastBlocks.header.height)

    val hv: Unit = newHistoryR.historyStorage.store.
      update(ByteArrayWrapper(lastBlocks.payload.id), IndexedSeq(ByteArrayWrapper(lastBlocks.header.id),
        ByteArrayWrapper(nextBlock.payload.id)), IndexedSeq())

    val newHistoryR1: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)

    val r = newHistoryR1.append(nextBlock.header).get._1
    val r1 = r.append(nextBlock.payload).get._1.reportModifierIsValid(nextBlock)

    println(r1.bestBlockHeight)

  }
}
