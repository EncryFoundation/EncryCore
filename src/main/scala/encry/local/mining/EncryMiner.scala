package encry.local.mining

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.pattern._
import akka.util.Timeout
import encry.consensus.{PowCandidateBlock, PowConsensus}
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{EncryBaseTransaction, TransactionFactory}
import encry.modifiers.state.box.{AmountCarryingBox, OpenBox}
import encry.settings.{Constants, EncryAppSettings}
import encry.view.history.{EncryHistory, Height}
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import io.circe.Json
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.{GetDataFromCurrentView, SemanticallySuccessfulModifier, Subscribe}
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.utils.Random

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class EncryMiner(settings: EncryAppSettings,
                 viewHolderRef: ActorRef,
                 nodeId: Array[Byte],
                 timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  import EncryMiner._

  private val startTime = timeProvider.time()

  private var isMining = false
  private var candidateOpt: Option[PowCandidateBlock] = None
  private var miningThreads: Seq[ActorRef] = Seq.empty

  override def preStart(): Unit = {
    viewHolderRef ! Subscribe(Seq(NodeViewHolder.EventType.SuccessfulSemanticallyValidModifier))
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(mod) =>
      if (isMining) {
        mod match {
          case f: EncryBlock if !candidateOpt.flatMap(_.parentOpt).exists(_.id sameElements f.header.id) =>
            prepareCandidate(settings, nodeId).foreach(_.foreach(c => self ! c))
          case _ =>
        }
      } else if (settings.nodeSettings.mining) {
        mod match {
          case f: EncryBlock if f.header.timestamp >= startTime =>
            self ! StartMining

          case _ =>
        }
      }

    case StartMining =>
      candidateOpt match {
        case Some(candidate) if !isMining && settings.nodeSettings.mining =>
          log.info("Starting Mining")
          miningThreads = Seq(context.actorOf(EncryMiningWorker.props(settings, viewHolderRef, candidate)))
          isMining = true
        case None =>
          context.system.scheduler.scheduleOnce(5.second) {
            prepareCandidate(settings, nodeId).foreach(_.foreach(c => {
              self ! c
              self ! StartMining
            }))
          }
        case _ =>
      }

    case c: PowCandidateBlock =>
      candidateOpt = Some(c)
      miningThreads.foreach(t => t ! c)

    case MiningStatusRequest =>
      sender ! MiningStatusResponse(isMining, candidateOpt)

    case m =>
      log.warn(s"Unexpected message $m")
  }

  def prepareCandidate(settings: EncryAppSettings, nodeId: Array[Byte]): Future[Option[PowCandidateBlock]] = {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    (viewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[PowCandidateBlock]] { v =>
      val history = v.history
      val state = v.state
      val pool = v.pool
      val vault = v.vault

      val bestHeaderOpt = history.bestBlockOpt.map(_.header)

      if ((bestHeaderOpt.isDefined || settings.nodeSettings.offlineGeneration) && vault.keyManager.keys.nonEmpty) Try {

        lazy val timestamp = timeProvider.time()
        val height = Height @@ (bestHeaderOpt.map(_.height).getOrElse(0) + 1)

        // `txsToPut` - valid, non-conflicting txs with respect to its fee amount.
        // `txsToDrop` - invalidated txs to be dropped from mempool.
        val (txsToPut, txsToDrop, _) = pool.takeAll.toSeq.sortBy(_.fee).reverse
          .foldLeft((Seq[EncryBaseTransaction](), Seq[EncryBaseTransaction](), Set[ByteArrayWrapper]())) {
            case ((validTxs, invalidTxs, bxsAcc), tx) =>
              val bxsRaw = tx.unlockers.map(u => ByteArrayWrapper(u.boxId))
              if ((validTxs.map(_.length).sum + tx.length) <= Constants.Chain.blockMaxSize - 124) {
                if (state.validate(tx).isSuccess && bxsRaw.forall(k => !bxsAcc.contains(k)) && bxsRaw.size == bxsRaw.toSet.size) {
                  (validTxs :+ tx, invalidTxs, bxsAcc ++ bxsRaw)
                } else {
                  (validTxs, invalidTxs :+ tx, bxsAcc)
                }
              } else {
                (validTxs, invalidTxs, bxsAcc)
              }
          }

        // Remove stateful-invalid txs from mempool.
        pool.removeAsync(txsToDrop)

        // TODO: Add ability to select key.
        val minerSecret = vault.keyManager.keys.head

        val openBxs: IndexedSeq[AmountCarryingBox] = txsToPut.foldLeft(IndexedSeq[OpenBox]())((buff, tx) =>
          buff ++ tx.newBoxes.foldLeft(IndexedSeq[OpenBox]()) { case (acc, bx) =>
            bx match {
              case obx: OpenBox => acc :+ obx
              case _ => acc
            }
          }) ++ vault.getAvailableCoinbaseBoxesAt(state.height)

        val coinbase = TransactionFactory.coinbaseTransactionScratch(minerSecret, timestamp, openBxs, height)

        val txs = txsToPut.sortBy(_.timestamp) :+ coinbase

        val (adProof, adDigest) = state.proofsForTransactions(txs).get
        val difficulty = bestHeaderOpt.map(parent => history.requiredDifficultyAfter(parent))
          .getOrElse(Constants.Chain.initialDifficulty)
        val derivedFields = PowConsensus.getDerivedHeaderFields(bestHeaderOpt, adProof, txs)
        val blockSignature = minerSecret.sign(
          EncryBlockHeader.getMessageToSign(derivedFields._1, minerSecret.publicImage, derivedFields._2,
            derivedFields._3, adDigest, derivedFields._4, timestamp, derivedFields._5, difficulty))

        val candidate = new PowCandidateBlock(minerSecret.publicImage,
          blockSignature, bestHeaderOpt, adProof, adDigest, txs, timestamp, difficulty)

        log.debug(s"Sending candidate block with ${candidate.transactions.length - 1} transactions " +
          s"and 1 coinbase for height $height")

        candidate
      }.recoverWith { case thr =>
        log.warn("Error when trying to generate candidate: ", thr)
        Failure(thr)
      }.toOption
      else {
        if (vault.keyManager.keys.isEmpty) vault.keyManager.initStorage(Random.randomBytes())
        None
      }
    }).mapTo[Option[PowCandidateBlock]]
  }
}


object EncryMiner extends ScorexLogging {

  case object StartMining

  case object MiningStatusRequest

  case class MiningStatusResponse(isMining: Boolean, candidateBlock: Option[PowCandidateBlock]) {
    lazy val json: Json = Map(
      "isMining" -> isMining.asJson,
      "candidateBlock" -> candidateBlock.map(_.json).getOrElse("None".asJson)
    ).asJson
  }
}

object EncryMinerRef {

  def props(settings: EncryAppSettings,
            viewHolderRef: ActorRef,
            nodeId: Array[Byte],
            timeProvider: NetworkTimeProvider): Props =
    Props(new EncryMiner(settings, viewHolderRef, nodeId, timeProvider))

  def apply(settings: EncryAppSettings,
            viewHolderRef: ActorRef,
            nodeId: Array[Byte],
            timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(settings, viewHolderRef, nodeId, timeProvider))

  def apply(settings: EncryAppSettings,
            viewHolderRef: ActorRef,
            nodeId: Array[Byte],
            timeProvider: NetworkTimeProvider,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(settings, viewHolderRef, nodeId, timeProvider), name)
}
