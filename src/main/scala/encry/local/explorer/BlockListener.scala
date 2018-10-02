package encry.local.explorer

import akka.actor.{Actor, ActorRef}
import encry.EncryApp.settings
import encry.utils.CoreTaggedTypes.ModifierId
import encry.local.explorer.BlockListener._
import encry.local.explorer.database.DBService
import encry.modifiers.history.Block.Height
import encry.modifiers.history.{Block, Header}
import encry.network.NodeViewSynchronizer.ReceivableMessages.ChangedHistory
import encry.utils.Logging
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetNodeViewChanges
import encry.view.history.EncryHistoryReader
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class BlockListener(dbService: DBService, readersHolder: ActorRef, nodeViewHolder: ActorRef) extends Actor with Logging {

  override def preStart(): Unit = logInfo(s"Start listening to new blocks.")

  override def postStop(): Unit = dbService.shutdown()

  val currentDbHeightFuture: Future[Int] = dbService.selectHeightOpt.map(_.getOrElse(0))
  val writingGap: Int = settings.postgres.flatMap(_.writingGap).getOrElse(20)

  currentDbHeightFuture.onComplete {
    case Success(height) =>
      if (height == 0) logInfo("Going to begin writing to empty database")
      else logInfo(s"Going to begin writing to table with $height blocks")
      nodeViewHolder ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = false)
    case Failure(th) =>
      logWarn(s"Failed to connect to database with exception $th")
      context.stop(self)
  }

  def orphanedAndChainSwitching: Receive = {
    case NewOrphaned(header: Header) => dbService.processOrphanedHeader(header)
    case ChainSwitching(ids) => dbService.markAsRemovedFromMainChain(ids.toList)
  }

  override def receive: Receive = orphanedAndChainSwitching.orElse {
    case ChangedHistory(history) => currentDbHeightFuture.map { dbHeight =>
      val currentHistoryHeight: Height = history.bestBlockOpt.map(_.header.height).getOrElse(0)
      if (dbHeight <= currentHistoryHeight && currentHistoryHeight >= writingGap) {
        context.become(uploadGap(history, history.bestBlockOpt.map(_.header.height).getOrElse(0) - writingGap))
        self ! UploadToDbOnHeight(if (dbHeight == 0) 0 else dbHeight + 1)
      } else context.become(operating(history, Vector.empty, dbHeight))
    }
    case NewBestBlock(_) =>
  }

  def operating(history: EncryHistoryReader, pending: Vector[Int] = Vector(), lastUploaded: Int): Receive =
    orphanedAndChainSwitching.orElse {
      case NewBestBlock(newHeight) =>
        nodeViewHolder ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = false)

        val newPending: Seq[Height] =
          if (newHeight != 0 && newHeight - writingGap - lastUploaded > 1) (lastUploaded + 1 until (newHeight - writingGap)).toVector
          else Vector[Int]()

        bestBlockOptAtHeight(history, newHeight - writingGap) match {
          case Some(block) if newPending.isEmpty => dbService.processBlock(block).foreach { _ =>
            self ! LastUploaded(newHeight - writingGap)
          }
          case Some(block) =>
            dbService.processBlock(block).foreach(_ => self ! LastUploaded(newHeight - writingGap))
            context.become(operating(history, pending ++ newPending, lastUploaded))
          case None if newHeight - writingGap >= 0 =>
            logInfo(s"Block on height ${newHeight - writingGap} not found, adding to pending list")
            context.become(operating(history, (pending :+ (newHeight - writingGap)) ++ newPending, lastUploaded))
          case None =>
        }
      case ChangedHistory(newHistory) =>
        if (pending.nonEmpty) context.become(operating(newHistory, tryToUploadPending(newHistory,pending), lastUploaded))
        else context.become(operating(newHistory, pending, lastUploaded))
      case LastUploaded(h) => context.become(operating(history, pending, h))
  }

  def uploadGap(history: EncryHistoryReader, currentHeight: Int, pending: Vector[Int] = Vector()): Receive =
    orphanedAndChainSwitching.orElse {
      case UploadToDbOnHeight(height) if height <= currentHeight =>
        bestBlockOptAtHeight(history, height).foreach { block =>
          dbService.processBlock(block).onComplete {
            case Success(_) =>
              self ! UploadToDbOnHeight(height + 1)
            case Failure(_) =>
              self ! UploadToDbOnHeight(height)
          }
        }
      case UploadToDbOnHeight(_) => nodeViewHolder ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = false)
      case ChangedHistory(newHistory) => context.become(operating(newHistory, pending, newHistory.bestBlockOpt.map(_.header.height).getOrElse(0)))
      case NewBestBlock(height) => context.become(uploadGap(history, currentHeight, pending :+ height))
    }

  def bestBlockOptAtHeight(history: EncryHistoryReader, height: Int): Option[Block] =
    history.headerIdsAtHeight(height)
      .headOption
      .flatMap(history.typedModifierById[Header])
      .flatMap(history.getBlock)

  def tryToUploadPending(history: EncryHistoryReader, pending: Vector[Int]): Vector[Int] =
    pending.map(height => (bestBlockOptAtHeight(history, height), height)).foldLeft(Vector[Int]()) {
      case (notFound, (blockOpt, height)) =>
        blockOpt match {
          case Some(block) =>
            logInfo(s"Pending block on height $height found, writing to postgres")
            dbService.processBlock(block)
            notFound
          case None => notFound :+ height
        }
      }

}

object BlockListener {
  case class NewBestBlock(onHeight: Int)
  case class ChainSwitching(switchedIds: Seq[ModifierId])
  case class NewOrphaned(header: Header)
  case class UploadToDbOnHeight(height: Int)
  case class LastUploaded(height: Int)
}
