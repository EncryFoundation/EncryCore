package encry.local.miner

import akka.actor.{Actor, ActorRef, Props, SupervisorStrategy}
import encry.EncryApp._
import encry.consensus._
import encry.consensus.emission.EncrySupplyController
import encry.crypto.PrivateKey25519
import encry.local.miner.EncryMiningWorker.{DropChallenge, NextChallenge}
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{EncryBaseTransaction, EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.AssetBox
import encry.modifiers.state.box.Box.Amount
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.Constants
import encry.utils.NetworkTime.Time
import encry.utils.ScorexLogging
import encry.view.EncryNodeViewHolder.CurrentView
import encry.view.EncryNodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}
import encry.view.history.{EncryHistory, Height}
import encry.view.mempool.EncryMempool
import encry.view.state.{StateMode, UtxoState}
import encry.view.wallet.EncryWallet
import io.circe.syntax._
import io.circe.{Encoder, Json}
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.authds.{ADDigest, SerializedAdProof}

import scala.collection._

class EncryMiner extends Actor with ScorexLogging {

  import EncryMiner._

  var candidateOpt: Option[CandidateBlock] = None
  var miningWorkers: Seq[ActorRef] = Seq.empty[ActorRef]

  override def preStart(): Unit = context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])

  override def postStop(): Unit = killAllWorkers()

  override def supervisorStrategy: SupervisorStrategy = commonSupervisorStrategy

  def killAllWorkers(): Unit = {
    miningWorkers.foreach(worker => context.stop(worker))
    miningWorkers = Seq.empty[ActorRef]
  }

  def needNewCandidate(b: EncryBlock): Boolean = !candidateOpt.flatMap(_.parentOpt).map(_.id).exists(_.sameElements(b.header.id))

  def shouldStartMine(b: EncryBlock): Boolean = settings.node.mining && b.header.timestamp >= timeProvider.time()

  def unknownMessage: Receive = {
    case m => log.warn(s"Unexpected message $m")
  }

  def mining: Receive = {

    case StartMining if miningWorkers.nonEmpty =>
      candidateOpt match {
        case Some(candidateBlock) =>
          miningWorkers.foreach(_ ! NextChallenge(candidateBlock))
        case None => produceCandidate()
      }

    case StartMining =>
      val numberOfWorkers: Int = settings.node.numberOfMiningWorkers
      miningWorkers = for (i <- 0 until numberOfWorkers) yield context.actorOf(
        Props(classOf[EncryMiningWorker], self, i, numberOfWorkers), s"worker$i")
      self ! StartMining

    case StopMining if miningWorkers.nonEmpty =>
      killAllWorkers()

    case MinedBlock(block) if candidateOpt.exists(_.stateRoot sameElements block.header.stateRoot) =>
      nodeViewHolder ! LocallyGeneratedModifier(block.header)
      nodeViewHolder ! LocallyGeneratedModifier(block.payload)
      if (settings.node.stateMode == StateMode.Digest) block.adProofsOpt.foreach { adp => nodeViewHolder ! LocallyGeneratedModifier(adp) }
      candidateOpt = None
      context.children.foreach(_ ! DropChallenge)

    case GetMinerStatus => sender ! MinerStatus(miningWorkers.nonEmpty, candidateOpt)

    case _ =>
  }

  def receiveSemanticallySuccessfulModifier: Receive = {
    /**
      * Case when we are already mining by the time modifier arrives and
      * get block from node view that has header's id which isn't equals to our candidate's parent id.
      * That means that our candidate is outdated. Should produce new candidate for ourselves.
      * Stop all current threads and re-run them with newly produced candidate.
      */
    case SemanticallySuccessfulModifier(mod: EncryBlock) if miningWorkers.nonEmpty && needNewCandidate(mod) => produceCandidate()

    /**
      * Non obvious but case when mining is enabled, but miner doesn't started yet. Initialization case.
      * We've received block that been generated by somebody else or genesis while we doesn't start.
      * And this block was generated after our miner had been started. That means that we are ready
      * to start mining.
      * This block could be either genesis or generated by another node.
      */
    case SemanticallySuccessfulModifier(mod: EncryBlock) if shouldStartMine(mod) => self ! StartMining
    case SemanticallySuccessfulModifier(_) =>
  }

  def receiverCandidateBlock: Receive = {
    case c: CandidateBlock => procCandidateBlock(c)
    case cEnv: CandidateEnvelope if cEnv.c.nonEmpty => procCandidateBlock(cEnv.c.get)
  }

  override def receive: Receive =
    receiveSemanticallySuccessfulModifier orElse
      receiverCandidateBlock orElse
      mining orElse
      unknownMessage

  def procCandidateBlock(c: CandidateBlock): Unit = {
    log.debug(s"Got candidate block $c")
    candidateOpt = Some(c)
    context.system.scheduler.scheduleOnce(settings.node.miningDelay, self, StartMining)
  }

  def createCandidate(view: CurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool],
                      bestHeaderOpt: Option[EncryBlockHeader]): CandidateBlock = {
    val timestamp: Time = timeProvider.time()
    val height = Height @@ (bestHeaderOpt.map(_.height).getOrElse(Constants.Chain.PreGenesisHeight) + 1)

    // `txsToPut` - valid, non-conflicting txs with respect to their fee amount.
    // `txsToDrop` - invalidated txs to be dropped from mempool.
    val (txsToPut, txsToDrop, _) = view.pool.takeAll.toSeq.sortBy(_.fee).reverse
      .foldLeft((Seq[EncryBaseTransaction](), Seq[EncryBaseTransaction](), Set[ByteArrayWrapper]())) {
        case ((validTxs, invalidTxs, bxsAcc), tx) =>
          val bxsRaw: IndexedSeq[ByteArrayWrapper] = tx.inputs.map(u => ByteArrayWrapper(u.boxId))
          if ((validTxs.map(_.length).sum + tx.length) <= Constants.BlockMaxSize - 124) {
            if (view.state.validate(tx).isSuccess && bxsRaw.forall(k => !bxsAcc.contains(k)) && bxsRaw.size == bxsRaw.toSet.size)
              (validTxs :+ tx, invalidTxs, bxsAcc ++ bxsRaw)
            else (validTxs, invalidTxs :+ tx, bxsAcc)
          } else (validTxs, invalidTxs, bxsAcc)
      }

    // Remove stateful-invalid txs from mempool.
    view.pool.removeAsync(txsToDrop)

    val minerSecret: PrivateKey25519 = view.vault.keyManager.mainKey
    val feesTotal: Amount = txsToPut.map(_.fee).sum
    val supplyBox: AssetBox = EncrySupplyController.supplyBoxAt(view.state.height)

    val coinbase: EncryTransaction = TransactionFactory
      .coinbaseTransactionScratch(minerSecret.publicImage, timestamp, IndexedSeq(supplyBox), feesTotal)

    val txs: Seq[TX] = txsToPut.sortBy(_.timestamp) :+ coinbase

    val (adProof: SerializedAdProof, adDigest: ADDigest) = view.state.generateProofs(txs)
      .getOrElse(throw new Exception("ADProof generation failed"))

    val difficulty: Difficulty = bestHeaderOpt.map(parent => view.history.requiredDifficultyAfter(parent))
      .getOrElse(Constants.Chain.InitialDifficulty)

    val candidate: CandidateBlock = CandidateBlock(bestHeaderOpt, adProof, adDigest, Constants.Chain.Version, txs, timestamp, difficulty)

    log.debug(s"Sending candidate block with ${candidate.transactions.length - 1} transactions " +
      s"and 1 coinbase for height $height")

    candidate
  }

  def produceCandidate(): Unit =
    nodeViewHolder ! GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, CandidateEnvelope] { view =>
      log.info("Starting candidate generation")
      val bestHeaderOpt: Option[EncryBlockHeader] = view.history.bestBlockOpt.map(_.header)
      if (bestHeaderOpt.isDefined || settings.node.offlineGeneration) CandidateEnvelope.fromCandidate(createCandidate(view, bestHeaderOpt))
      else CandidateEnvelope.empty
    }
}

object EncryMiner extends ScorexLogging {

  case object StartMining

  case object StopMining

  case object GetMinerStatus

  case class MinedBlock(block: EncryBlock)

  case class MinerStatus(isMining: Boolean, candidateBlock: Option[CandidateBlock]) {
    lazy val json: Json = Map(
      "isMining" -> isMining.asJson,
      "candidateBlock" -> candidateBlock.map(_.asJson).getOrElse("None".asJson)
    ).asJson
  }

  case class CandidateEnvelope(c: Option[CandidateBlock])

  object CandidateEnvelope {

    val empty: CandidateEnvelope = CandidateEnvelope(None)

    def fromCandidate(c: CandidateBlock): CandidateEnvelope = CandidateEnvelope(Some(c))
  }

  implicit val jsonEncoder: Encoder[MinerStatus] = (r: MinerStatus) =>
    Map(
      "isMining" -> r.isMining.asJson,
      "candidateBlock" -> r.candidateBlock.map(_.asJson).getOrElse("None".asJson)
    ).asJson
}


