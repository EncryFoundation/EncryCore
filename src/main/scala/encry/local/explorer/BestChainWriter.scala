package encry.local.explorer

import akka.actor.{Actor, ActorSelection}
import encry.utils.Logging
import encry.local.explorer.BestChainWriter.{HeaderForDBForks, SetNewHeight}
import encry.local.explorer.database.DBService
import encry.modifiers.history.Header
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, SyntacticallySuccessfulModifier}
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetNodeViewChanges
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class BestChainWriter(dbService: DBService) extends Actor with Logging {

  val nodeViewHolderRef: ActorSelection = context.actorSelection("/user/nodeViewHolder")
  val currentForkHeight: Future[Int] = dbService.selectHeightFromFork.map(_.getOrElse(0))
  val gap: Int = 10

  currentForkHeight.onComplete {
    case Success(height) => logInfo(s"Starting write forks to postgres with height: $height")
      self ! SetNewHeight(height)
      nodeViewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = false)
    case Failure(ex) =>
      logError(s"Failed to connect to DB. Actor will be stopped with exception: ${ex.getStackTrace}")
      context.stop(self)
  }

  override def receive: Receive = receiveHandler()

  def receiveHandler(lastWrittenHeight: Int = 0): Receive = {
    case SetNewHeight(height) => println(height)
      context.become(receiveHandler(height))
    case ChangedHistory(history) => currentForkHeight.map { height =>
      println(s"Height from DB is: $lastWrittenHeight")
      val diffNew = history.bestHeaderHeight - lastWrittenHeight
      println(s"New difference between bestHeight and HeightFromDB is: $diffNew")
      val lastHeadersForWrite: List[Header] =
        history
          .lastHeaders(diffNew)
          .headers
          .dropRight(gap)
          .toList
      lastHeadersForWrite.foreach(x => println(s"Headers for write ${x.height} and ${x.encodedId}"))

      lastHeadersForWrite.foldLeft(Future.successful(1)) { case (prevList, headerNew) =>
        prevList.flatMap { _ =>
          val headerForDb: HeaderForDBForks = HeaderForDBForks(headerNew.encodedId, headerNew.height)
          dbService.insertHeadersFromNode(headerForDb)
        }
      }

//      lastHeadersForWrite
//        .map(header => HeaderForDBForks(header.encodedId, header.height))
//        .foreach { header =>
//          dbService.insertHeadersFromNode(header).onComplete {
//            case Success(_) => println(s"header with height: ${header.height} successful written")
//            case Failure(ex) =>
//              println(s"While writing header with height: ${header.height} exception ${ex.getStackTrace}"
//                + s" has become. Start writing headers from height ${header.height}")

//          }

//        }

      context.become(receiveHandler(lastHeadersForWrite.lastOption.map(_.height).getOrElse(lastWrittenHeight)))
    }

    case SyntacticallySuccessfulModifier(mod) => mod match {
      case header: Header =>
        nodeViewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = false)
      case _ =>
    }
  }
}

object BestChainWriter {

  case class HeaderForDBForks(id: String, height: Int)

  case class SetNewHeight(height: Int)

}