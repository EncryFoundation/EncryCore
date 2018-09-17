package encry.local.explorer

import akka.actor.Actor
import encry.utils.CoreTaggedTypes.ModifierId
import encry.local.explorer.BlockListener.{ChainSwitching, NewOrphaned}
import encry.local.explorer.database.DBService
import encry.modifiers.history.{Block, Header}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.utils.Logging
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class BlockListener(dBService: DBService) extends Actor with Logging {

  override def preStart(): Unit = {
    logInfo(s"Start listening to new blocks.")
    context.system.eventStream.subscribe(context.self, classOf[SemanticallySuccessfulModifier[_]])
  }

  val currentHeightOptFuture: Future[Option[Int]] = dBService.selectHeightOpt

  currentHeightOptFuture.onComplete {
    case Success(heightOpt) => heightOpt match {
      case Some(height) => logInfo(s"Going to begin writing to table with $height blocks")
      case None => logInfo("Going to begin writing to empty database")
    }
    case Failure(th) =>
      logWarn(s"Failed to connect to database with exception $th")
      context.stop(self)
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(block: Block) => dBService.processBlock(block)
    case NewOrphaned(header: Header) => dBService.processOrphanedHeader(header)
    case SemanticallySuccessfulModifier(block: EncryBlock) =>
      currentHeightOptFuture.map(heightOpt => if(heightOpt.forall(_ < block.header.height)) dBService.processBlock(block))
    case NewOrphaned(header: EncryBlockHeader) => dBService.processOrphanedHeader(header)
    case ChainSwitching(ids) => dBService.markAsRemovedFromMainChain(ids.toList)
  }
}

object BlockListener {
  case class ChainSwitching(switchedIds: Seq[ModifierId])
  case class NewOrphaned(header: Header)
}
