package encry.local.explorer

import akka.actor.{Actor, ActorRef}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.local.explorer.BlockListener.{ChainSwitching, NewOrphaned, UploadToDbOnHeight}
import encry.local.explorer.database.DBService
import encry.modifiers.history.{Block, Header}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.{ChangedHistory, SemanticallySuccessfulModifier}
import encry.utils.Logging
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetNodeViewChanges
import encry.view.history.EncryHistoryReader
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class BlockListener(dbService: DBService, readersHolder: ActorRef, nodeViewHolder: ActorRef) extends Actor with Logging {

  override def preStart(): Unit = {
    logInfo(s"Start listening to new blocks.")
    context.system.eventStream.subscribe(context.self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def postStop(): Unit = dbService.shutdown()

  val currentDbHeightFuture: Future[Int] = dbService.selectHeightOpt.map(_.getOrElse(0))

  currentDbHeightFuture.onComplete {
    case Success(height) =>
      if (height == 0) logInfo("Going to begin writing to empty database")
      else logInfo(s"Going to begin writing to table with $height blocks")
      nodeViewHolder ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = false)
    case Failure(th) =>
      logWarn(s"Failed to connect to database with exception $th")
      context.stop(self)
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(block: Block) =>
      currentDbHeightFuture.map(dbHeight => if(dbHeight < block.header.height || block.header.height == 0) dbService.processBlock(block))
    case NewOrphaned(header: Header) => dbService.processOrphanedHeader(header)
    case ChainSwitching(ids) => dbService.markAsRemovedFromMainChain(ids.toList)
    case ChangedHistory(history) => currentDbHeightFuture.map { dbHeight =>
      if (dbHeight <= history.bestBlockOpt.map(_.header.height).getOrElse(0)) {
        context.become(receiveWithNewHistory(history, history.bestBlockOpt.map(_.header.height).getOrElse(0)))
        self ! UploadToDbOnHeight(if (dbHeight == 0) 0 else dbHeight + 1)
      }
    }
  }

  def receiveWithNewHistory(history: EncryHistoryReader, currentHeight: Int): Receive = receive.orElse {
    case UploadToDbOnHeight(height) if height <= currentHeight =>
      import history.{headerIdsAtHeight, typedModifierById, getBlock}

      headerIdsAtHeight(height).headOption.flatMap(typedModifierById[Header]).flatMap(getBlock).foreach { block =>
        dbService.processBlock(block).onComplete {
          case Success(_) =>
            self ! UploadToDbOnHeight(height + 1)
          case Failure(_) =>
            self ! UploadToDbOnHeight(height)
        }
      }
    case UploadToDbOnHeight(_) => context.become(receive)
  }

}

object BlockListener {
  case class ChainSwitching(switchedIds: Seq[ModifierId])
  case class NewOrphaned(header: Header)
  case class UploadToDbOnHeight(height: Int)
}
