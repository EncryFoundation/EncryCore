package encry.local.mining

import akka.actor.{Actor, ActorRef}
import akka.pattern._
import akka.util.Timeout
import encry.consensus.{Difficulty, PowCandidateBlock, PowConsensus}
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{CoinbaseTransaction, EncryBaseTransaction}
import encry.modifiers.state.box.OpenBox
import encry.settings.EncryAppSettings
import encry.view.history.{EncryHistory, Height}
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.{GetDataFromCurrentView, SemanticallySuccessfulModifier, Subscribe}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.utils.Random

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class EncryMiner(viewHolderRef: ActorRef,
                 settings: EncryAppSettings,
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
    implicit val timeout = Timeout(settings.scorexSettings.restApi.timeout)
    (viewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[PowCandidateBlock]] { v =>
      val history = v.history
      val state = v.state
      val pool = v.pool
      val vault = v.vault

      val bestHeaderOpt = history.bestFullBlockOpt.map(_.header)

      if ((bestHeaderOpt.isDefined || settings.nodeSettings.offlineGeneration) &&
        !pool.isEmpty &&
        vault.keyManager.keys.nonEmpty) Try {

        lazy val timestamp = timeProvider.time()
        val height = Height @@ (bestHeaderOpt.map(_.height).getOrElse(0) + 1)

        var txs = state.filterValid(pool.takeAllUnordered.toSeq)
          .foldLeft(Seq[EncryBaseTransaction]()) { case (txsBuff, tx) =>
            // 124 is approximate CoinbaseTx.length in bytes.
            if ((txsBuff.map(_.length).sum + tx.length) <= settings.chainSettings.blockMaxSize - 124) txsBuff :+ tx
            else txsBuff
          }

        // TODO: Which PubK should we pick here?
        val minerProposition = vault.publicKeys.head
        val privateKey: PrivateKey25519 = vault.secretByPublicImage(minerProposition).get

        val openBxs: IndexedSeq[OpenBox] = txs.foldLeft(IndexedSeq[OpenBox]())((buff, tx) =>
          buff ++ tx.newBoxes.foldLeft(IndexedSeq[OpenBox]()) { case (buff2, bx) =>
            bx match {
              case obx: OpenBox => buff2 :+ obx
              case _ => buff2
            }
          }) ++ state.getAvailableOpenBoxesAt(state.stateHeight)
        val amount = openBxs.map(_.amount).sum
        val cTxSignature = PrivateKey25519Companion.sign(privateKey,
          CoinbaseTransaction.getHash(minerProposition, openBxs.map(_.id), timestamp, amount, height))
        val coinbase =
          CoinbaseTransaction(minerProposition, timestamp, cTxSignature, openBxs.map(_.id), amount, height)

        txs = txs.sortBy(_.timestamp) :+ coinbase

        val (adProof, adDigest) = state.proofsForTransactions(txs).get
        val difficulty = bestHeaderOpt.map(parent => history.requiredDifficultyAfter(parent))
          .getOrElse(Difficulty @@ settings.chainSettings.initialDifficulty)
        val derivedFields = PowConsensus.getDerivedHeaderFields(bestHeaderOpt, adProof, txs)
        val blockSignature = PrivateKey25519Companion.sign(privateKey,
          EncryBlockHeader.getMessageToSign(derivedFields._1, minerProposition, derivedFields._2,
            derivedFields._3, adDigest, derivedFields._4, timestamp, derivedFields._5, difficulty))

        val candidate = new PowCandidateBlock(minerProposition,
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
