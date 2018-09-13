package encry.local.explorer

import akka.actor.Actor
import encry.utils.CoreTaggedTypes.ModifierId
import encry.local.explorer.BlockListener.{ChainSwitching, NewOrphaned}
import encry.local.explorer.database.DBService
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.utils.Logging
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

class BlockListener(dBService: DBService) extends Actor with Logging {

  override def preStart(): Unit = {
    logInfo(s"Start listening to new blocks.")
    context.system.eventStream.subscribe(context.self, classOf[SemanticallySuccessfulModifier[_]])
  }

  val currentHeightOptFuture: Future[Option[Int]] = dBService.selectHeightOpt

  currentHeightOptFuture.recover {
    case NonFatal(th) =>
      logWarn(s"Failed to connect to database with exception $th")
      context.stop(self)
      Some(Int.MaxValue)
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(block: EncryBlock) =>
      currentHeightOptFuture.map(heightOpt => if(heightOpt.forall(_ < block.header.height)) dBService.processBlock(block))
    case NewOrphaned(header: EncryBlockHeader) => dBService.processOrphanedHeader(header)
    case ChainSwitching(ids) => dBService.markAsRemovedFromMainChain(ids.toList)
  }
}

object BlockListener {
  case class ChainSwitching(switchedIds: Seq[ModifierId])
  case class NewOrphaned(header: EncryBlockHeader)
}
