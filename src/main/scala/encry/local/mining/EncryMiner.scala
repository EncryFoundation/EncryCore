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
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import io.circe.Json
import io.circe.syntax._
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.{GetDataFromCurrentView, SemanticallySuccessfulModifier, Subscribe}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.crypto.encode.Base16

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class EncryMiner(viewHolderRef: ActorRef, settings: EncryAppSettings,
                 nodeId: Array[Byte], timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  import EncryMiner._

  private val consensus = new PowConsensus(settings.chainSettings)

  private var isMining = false
  private val startTime = timeProvider.time()
  private var nonce = 0
  private var candidateOpt: Option[PowCandidateBlock] = None

  override def preStart(): Unit = {
    viewHolderRef ! Subscribe(Seq(NodeViewHolder.EventType.SuccessfulSemanticallyValidModifier))
  }

  override def receive: Receive = {

    case SemanticallySuccessfulModifier(mod) =>
      if (isMining) {
        mod match {
          case block: EncryBlock =>
            if (!candidateOpt.flatMap(_.parentOpt).exists(_.id sameElements block.header.id))
              prepareCandidate(viewHolderRef, settings, nodeId, timeProvider)

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
      if (!isMining && settings.nodeSettings.mining) {
        log.info("Starting Mining")
        isMining = true
        self ! MineBlock
      }

    case StopMining =>
      isMining = false

    case PrepareCandidate =>
      val cOpt = prepareCandidate(viewHolderRef, settings, nodeId, timeProvider)
      cOpt onComplete(opt => candidateOpt = opt.get)

    case MineBlock =>
      nonce = nonce + 1
      candidateOpt match {
        case Some(candidate) =>
          consensus.verifyCandidate(candidate, nonce) match {
            case Some(block) =>
              log.info(s"New block found: $block")

              viewHolderRef ! LocallyGeneratedModifier(block.header)
              viewHolderRef ! LocallyGeneratedModifier(block.payload)
              block.adProofsOpt.foreach { adp =>
                viewHolderRef ! LocallyGeneratedModifier(adp)
              }
              candidateOpt = None
              context.system.scheduler.scheduleOnce(settings.nodeSettings.miningDelay)(self ! MineBlock)
            case None =>
              if (isMining) self ! MineBlock
          }
        case None =>
          log.info("Candidate is empty. Trying again in 1 sec.")
          context.system.scheduler.scheduleOnce(1.second)(self ! PrepareCandidate)
          context.system.scheduler.scheduleOnce(2.second)(self ! MineBlock)
      }

    case MiningStatusRequest =>
      sender ! MiningStatusResponse(isMining, candidateOpt)
  }
}

object EncryMiner extends ScorexLogging {

  case object StartMining

  case object StopMining

  case object MineBlock

  case object PrepareCandidate

  case object MiningStatusRequest

  case class MiningStatusResponse(isMining: Boolean, candidateBlock: Option[PowCandidateBlock]) {
    lazy val json: Json = Map(
      "isMining" -> isMining.asJson,
      "candidateBlock" -> candidateBlock.map(_.json).getOrElse("None".asJson)
    ).asJson
  }

  def prepareCandidate(viewHolderRef: ActorRef, settings: EncryAppSettings,
                       nodeId: Array[Byte], timeProvider: NetworkTimeProvider): Future[Option[PowCandidateBlock]] = {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    (viewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[PowCandidateBlock]] { view =>
        val bestHeaderOpt = view.history.bestFullBlockOpt.map(_.header)

        if (bestHeaderOpt.isDefined) {
          log.debug("BestHeader id:        " + Base16.encode(bestHeaderOpt.get.hHash))
          log.debug("BestHeader timestamp: " + bestHeaderOpt.get.timestamp)
          log.debug("BestHeader height:    " + bestHeaderOpt.get.height)
        } else {
          log.debug("BestHeader is undefined")
        }

        if ((bestHeaderOpt.isDefined || settings.nodeSettings.offlineGeneration) && !view.pool.isEmpty) Try {

          // TODO: Should this be lazy?
          lazy val timestamp = timeProvider.time()

          var txs = view.state.filterValid(view.pool.takeAllUnordered.toSeq)
            .foldLeft(Seq[EncryBaseTransaction]()) { case (txsBuff, tx) =>
              // 124 is approximate CoinbaseTx.length in bytes.
              if ((txsBuff.map(_.length).sum + tx.length) <= settings.chainSettings.blockMaxSize - 124) txsBuff :+ tx
              else txsBuff
            }

          // TODO: Which PubK should we pick here?
          val minerProposition = view.vault.publicKeys.head
          val privateKey: PrivateKey25519 = view.vault.secretByPublicImage(minerProposition).get
          val openBxs = txs.flatMap(tx => tx.newBoxes.filter(_.isInstanceOf[OpenBox])).toIndexedSeq
          val amount = openBxs.map(_.value).sum
          val cTxSignature = PrivateKey25519Companion.sign(privateKey,
            CoinbaseTransaction.getHash(minerProposition, openBxs.map(_.id), timestamp, amount))

          val coinbase = CoinbaseTransaction(minerProposition, timestamp, cTxSignature, openBxs.map(_.id), amount)

          txs = txs.sortBy(_.timestamp) :+ coinbase

          val (adProof, adDigest) = view.state.proofsForTransactions(txs).get
          val difficulty = bestHeaderOpt.map(parent => view.history.requiredDifficultyAfter(parent))
            .getOrElse(Difficulty @@ settings.chainSettings.initialDifficulty)

          val derivedFields = PowConsensus.getDerivedHeaderFields(bestHeaderOpt, adProof, txs)
          val blockSignature = PrivateKey25519Companion.sign(privateKey,
            EncryBlockHeader.getMessageToSign(derivedFields._1, minerProposition, derivedFields._2,
              derivedFields._3, adDigest, derivedFields._4, timestamp, derivedFields._5, difficulty))

          val candidate = new PowCandidateBlock(minerProposition, blockSignature, bestHeaderOpt, adProof, adDigest, txs, timestamp, difficulty)
          log.debug(s"Sending candidate block with ${candidate.transactions.length} transactions")

          candidate
        }.recoverWith { case thr =>
          log.warn("Error when trying to generate candidate: ", thr)
          Failure(thr)
        }.toOption
        else
          None
    }).mapTo[Option[PowCandidateBlock]]
  }
}
