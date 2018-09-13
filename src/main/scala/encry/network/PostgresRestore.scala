package encry.network

import akka.actor.Actor
import akka.persistence.RecoveryCompleted
import encry.EncryApp.{nodeViewHolder, peerManager, settings}
import encry.utils.Logging
import encry.local.explorer.database.DBService
import encry.modifiers.history.{ADProofSerializer, ADProofs}
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.HeaderDBVersion
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.directive.DirectiveDBVersion
import encry.modifiers.mempool.{InputDBVersion, Transaction, TransactionDBVersion}
import encry.view.EncryNodeViewHolder.ReceivableMessages.BlocksFromLocalPersistence
import org.encryfoundation.common.transaction.ProofSerializer
import scorex.crypto.encode.Base16
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

class PostgresRestore(dbService: DBService) extends Actor with Logging {

  val heightFuture: Future[Int] = dbService.selectHeight

  heightFuture.onComplete {
    case Success(height) => logInfo(s"Going to download $height blocks from postgres")
    case Failure(_) =>
      peerManager ! RecoveryCompleted
      nodeViewHolder ! BlocksFromLocalPersistence(Seq.empty, true)
      context.stop(self)
  }

  override def receive: Receive = {
    case StartRecovery => startRecovery().map { _ =>
      logInfo(s"All blocks restored from postgres")
      context.stop(self)
    }
  }

  def startRecovery(): Future[Unit] = heightFuture.flatMap { height =>
    settings.postgres.restoreBatchSize match {
      case Some(step) =>
        (0 to height).sliding(step, step).foldLeft(Future.successful(List[EncryBlock]())) { case (prevBlocks, slide) =>
          val from: Int = slide.head
          val to: Int = slide.last
          prevBlocks.flatMap { retrievedBlocks =>
            nodeViewHolder ! BlocksFromLocalPersistence(retrievedBlocks, to == height)
            selectBlocksByRange(from, to)
              .recoverWith {
                case NonFatal(th) =>
                  logWarn(s"Failure during restore: ${th.getLocalizedMessage}")
                  Future.failed(th)
              }
          }
        }.map(_ => Unit)
      case None => Future.unit
    }
  }

  private def selectBlocksByRange(from: Int, to: Int): Future[List[EncryBlock]] = {
    val headersFuture: Future[List[HeaderDBVersion]] = dbService.headersByRange(from, to)
    val txsFuture: Future[List[TransactionDBVersion]] = dbService.txsByRange(from, to)
    for {
      headers    <- headersFuture
        .map {
          _.map(h => (h.toHeader, parseAdProofs(h.adProofOpt))).collect {
            case (Success(header), Success(proofs)) => Try((header, proofs))
          }.map(Future.fromTry)
        }.flatMap(fs => Future.sequence(fs))
      txs        <- txsFuture
      inputs     <- dbService.inputsByTxIds(txs.map(_.id))
      directives <- dbService.directivesByTxIds(txs.map(_.id))
    } yield {
      val groupedInputs: Map[String, List[InputDBVersion]] = inputs.groupBy(_.txId)
      val groupedDirectives: Map[String, List[DirectiveDBVersion]] = directives.groupBy(_.txId)
      val txsWithIO: Map[String, List[Transaction]] = txs.groupBy(_.blockId).mapValues {
        _.sortBy(_.number).map { tx =>
          Transaction(
            tx.fee,
            tx.timestamp,
            groupedInputs.getOrElse(tx.id, IndexedSeq.empty).sortBy(_.numberInTx).flatMap(_.toInput.toOption).toIndexedSeq,
            groupedDirectives.getOrElse(tx.id, List.empty).sortBy(_.numberInTx).flatMap(_.toDirective).toIndexedSeq,
            tx.proof.flatMap(Base16.decode(_).toOption).flatMap(ProofSerializer.parseBytes(_).toOption)
          )
        }
      }
      headers.map { case (header, adProofOpt) =>
        EncryBlock(header, EncryBlockPayload(header.id, txsWithIO.getOrElse(Base16.encode(header.id), Seq.empty)), adProofOpt)
      }
    }
  }

  private def parseAdProofs(serializedOpt: Option[String]): Try[Option[ADProofs]] = serializedOpt match {
    case None => Success(None)
    case Some(serialized) =>
      Base16
        .decode(serialized)
        .flatMap(ADProofSerializer.parseBytes)
        .map(Option(_))
  }

}

case object StartRecovery
