package encry.network

import akka.actor.{Actor, ActorRef}
import akka.persistence.RecoveryCompleted
import com.typesafe.scalalogging.StrictLogging
import encry.local.explorer.database.DBService
import encry.modifiers.history.{ADProofSerializer, ADProofs}
import encry.modifiers.history.{Block, HeaderDBVersion, Payload}
import encry.modifiers.mempool.directive.DirectiveDBVersion
import encry.modifiers.mempool.{InputDBVersion, Transaction, TransactionDBVersion}
import encry.network.NodeViewSynchronizer.ReceivableMessages.ChangedHistory
import encry.settings.EncryAppSettings
import encry.view.EncryNodeViewHolder.ReceivableMessages.{BlocksFromLocalPersistence, GetNodeViewChanges}
import org.encryfoundation.common.transaction.ProofSerializer
import scorex.crypto.encode.Base16

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

class PostgresRestore(dbService: DBService, settings: EncryAppSettings, peerManager: ActorRef)
  extends Actor with StrictLogging {

  val heightFuture: Future[Int] = dbService.selectHeightOpt.map(_.getOrElse(0))
  var nvhOpt: Option[ActorRef] = None

  override def postStop(): Unit = if (settings.postgres.exists(_.enableSave)) Unit else dbService.shutdown()

  heightFuture.onComplete {
    case Success(0) =>
      logger.info("Seems like postgres is empty")
      context.stop(self)
    case Success(height) =>
      logger.info(s"Going to download $height blocks from postgres")
    case Failure(_) =>
      logger.warn("Failed to connect to postgres")
      peerManager ! RecoveryCompleted
      context.stop(self)
  }

  override def receive: Receive = {
    case nvh: ActorRef =>
      nvhOpt = Some(nvh)
      heightFuture.recoverWith {
        case NonFatal(th) =>
          nvh ! BlocksFromLocalPersistence(Seq.empty, true)
          Future.failed(th)
      }
      nvh ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = false)
    case ChangedHistory(history) =>
      val currentNodeHeight = history.bestBlockOpt.map(_.header.height).getOrElse(0)
      startRecovery(if (currentNodeHeight == 0) 0 else currentNodeHeight + 1).map { _ =>
        logger.info(s"All blocks restored from postgres")
        context.stop(self)
      }
  }

  def startRecovery(startFrom: Int): Future[Unit] = heightFuture.flatMap { height =>
    if (height <= startFrom) {
      nvhOpt.foreach(_ ! BlocksFromLocalPersistence(Seq.empty, true))
      Future.unit
    } else settings.postgres.flatMap(_.restoreBatchSize) match {
      case Some(step) =>
        logger.info(s"Going to download blocks from postgres starting from $startFrom")
        (startFrom to height).sliding(step, step).foldLeft(Future.successful(List[Block]())) { case (prevBlocks, slide) =>
          val from: Int = slide.head
          val to: Int = slide.last
          prevBlocks.flatMap { retrievedBlocks =>
            nvhOpt.foreach(_ ! BlocksFromLocalPersistence(retrievedBlocks, to == height))
            selectBlocksByRange(from, to)
              .recoverWith {
                case NonFatal(th) =>
                  logger.warn(s"Failure during restore: ${th.getLocalizedMessage}")
                  Future.failed(th)
              }
          }
        }.map(_ => Unit)
      case None => Future.unit
    }
  }

  private def selectBlocksByRange(from: Int, to: Int): Future[List[Block]] = {
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
        Block(header, Payload(header.id, txsWithIO.getOrElse(Base16.encode(header.id), Seq.empty)), adProofOpt)
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