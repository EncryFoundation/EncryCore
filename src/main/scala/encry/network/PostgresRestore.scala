package encry.network

import akka.actor.Actor
import akka.persistence.RecoveryCompleted
import encry.EncryApp.peerManager
import encry.utils.Logging
import encry.local.explorer.database.DBService
import encry.modifiers.history.block.{EncryBlock, payload}
import encry.modifiers.history.block.header.HeaderDBVersion
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.directive.DirectiveDBVersion
import encry.modifiers.mempool.{Transaction, InputDBVersion, TransactionDBVersion}
import scorex.crypto.encode.Base16
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class PostgresRestore(dBService: DBService) extends Actor with Logging {

  val heightFuture: Future[Int] = dBService.selectHeight

  heightFuture.onComplete {
    case Success(_) =>
    case Failure(_) => peerManager ! RecoveryCompleted
  }

  override def receive: Receive = {
    case _ =>
  }

  def startRecovery(): Future[Unit] = heightFuture.map { height =>

  }

  private def selectBlocksByRange(from: Int, to: Int): Future[List[EncryBlock]] = {
    val headersFuture: Future[List[HeaderDBVersion]] = dBService.headersByRange(from, to)
    val txsFuture: Future[List[TransactionDBVersion]] = dBService.txsByRange(from, to)
    for {
      headers <- headersFuture
        .map(_.map(_.toHeader).map(Future.fromTry))
        .flatMap(fs => Future.sequence(fs))
      txs <- txsFuture
      inputs <- dBService.inputsByTxIds(txs.map(_.id))
      directives <- dBService.directivesByTxIds(txs.map(_.id))
    } yield {
      val groupedInputs: Map[String, List[InputDBVersion]] = inputs.groupBy(_.txId)
      val groupedDirectives: Map[String, List[DirectiveDBVersion]] = directives.groupBy(_.txId)
      val txsWithIO: Map[String, List[Transaction]] = txs.groupBy(_.blockId).mapValues {
        _.map { tx =>
          Transaction(
            tx.fee,
            tx.timestamp,
            groupedInputs.getOrElse(tx.id, IndexedSeq.empty).flatMap(_.toInput.toOption).toIndexedSeq,
            groupedDirectives.getOrElse(tx.id, List.empty).flatMap(_.toDirective).toIndexedSeq,
            None)
        }
      }
      headers.map { header =>
        EncryBlock(header, EncryBlockPayload(header.id, txsWithIO.getOrElse(Base16.encode(header.id), Seq.empty)), None)
      }
    }
  }

}
