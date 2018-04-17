package encry.local.miner

import akka.actor.{Actor, ActorRef, ActorRefFactory, PoisonPill, Props}
import akka.pattern._
import akka.util.Timeout
import encry.consensus.{PowCandidateBlock, PowConsensus}
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{EncryBaseTransaction, TransactionFactory}
import encry.modifiers.state.box.{AmountCarryingBox, AssetBox}
import encry.settings.{Constants, EncryAppSettings}
import encry.view.history.{EncryHistory, Height}
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import io.circe.Json
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.utils.Random

import scala.collection._
import scala.collection.mutable.ArrayBuffer
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
  private val workers: mutable.Buffer[ActorRef] = new ArrayBuffer[ActorRef]()

  override def preStart(): Unit = context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])

  override def postStop(): Unit = killAllWorkers()

  private def killAllWorkers(): Unit = {
    log.warn("Stopping miner's threads.")
    workers.foreach( _ ! PoisonPill)
    workers.clear()
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(mod) =>
      if (isMining) {
        mod match {
          case block: EncryBlock if !candidateOpt.flatMap(_.parentOpt).exists(_.id sameElements block.header.id) =>
            prepareCandidate(settings, nodeId).foreach(_.foreach(c => self ! c))
          case _ =>
        }
      } else if (settings.nodeSettings.mining) {
        mod match {
          case block: EncryBlock if block.header.timestamp >= startTime =>
            self ! StartMining
          case _ =>
        }
      }

    case StartMining =>
      candidateOpt match {
        case Some(candidate) if !isMining && settings.nodeSettings.mining =>
          log.info("Starting Mining")
          isMining = true
          workers += EncryMiningWorker(settings, viewHolderRef, candidate)(context)
          workers.foreach(_ ! candidateOpt.get)
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
      workers.foreach(t => t ! c)

    case GetMinerStatus =>
      sender ! MinerStatus(isMining, candidateOpt)

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
        val height = Height @@ (bestHeaderOpt.map(_.height).getOrElse(Constants.Chain.PreGenesisHeight) + 1)

        // `txsToPut` - valid, non-conflicting txs with respect to its fee amount.
        // `txsToDrop` - invalidated txs to be dropped from mempool.
        val (txsToPut, txsToDrop, _) = pool.takeAll.toSeq.sortBy(_.fee).reverse
          .foldLeft((Seq[EncryBaseTransaction](), Seq[EncryBaseTransaction](), Set[ByteArrayWrapper]())) {
            case ((validTxs, invalidTxs, bxsAcc), tx) =>
              val bxsRaw = tx.unlockers.map(u => ByteArrayWrapper(u.boxId))
              if ((validTxs.map(_.length).sum + tx.length) <= Constants.Chain.BlockMaxSize - 124) {
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

        val minerSecret = vault.keyManager.mainKey

        val openBxs: IndexedSeq[AmountCarryingBox] = txsToPut.foldLeft(IndexedSeq[AssetBox]())((buff, tx) =>
          buff ++ tx.newBoxes.foldLeft(IndexedSeq[AssetBox]()) { case (acc, bx) =>
            bx match {
              case ab: AssetBox if ab.isOpen => acc :+ ab
              case _ => acc
            }
          }) ++ vault.getAvailableCoinbaseBoxesAt(state.height)

        val coinbase = TransactionFactory.coinbaseTransactionScratch(minerSecret, timestamp, openBxs, height)

        val txs = txsToPut.sortBy(_.timestamp) :+ coinbase

        val (adProof, adDigest) = state.proofsForTransactions(txs).get
        val difficulty = bestHeaderOpt.map(parent => history.requiredDifficultyAfter(parent))
          .getOrElse(Constants.Chain.InitialDifficulty)
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

  case object GetMinerStatus

  case class MinerStatus(isMining: Boolean, candidateBlock: Option[PowCandidateBlock]) {
    lazy val json: Json = Map(
      "isMining" -> isMining.asJson,
      "candidateBlock" -> candidateBlock.map(_.asJson).getOrElse("None".asJson)
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
