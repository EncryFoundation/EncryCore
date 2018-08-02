package encry.local.miner

import java.text.SimpleDateFormat
import java.util.Date
import akka.actor.{Actor, Props}
import encry.EncryApp._
import encry.consensus._
import encry.crypto.PrivateKey25519
import encry.local.miner.EncryMiningWorker.{DropChallenge, NextChallenge}
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{BaseTransaction, EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.Box.Amount
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.Constants
import encry.stats.StatsSender.{CandidateProducingTime, MiningEnd, MiningTime, SleepTime}
import encry.utils.Logging
import encry.utils.NetworkTime.Time
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

class EncryMiner extends Actor with Logging {

  import EncryMiner._

  val dateFormat: SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")
  var startTime: Long = System.currentTimeMillis()
  var sleepTime: Long = System.currentTimeMillis()
  var candidateOpt: Option[CandidateBlock] = None

  override def preStart(): Unit = context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])

  override def postStop(): Unit = killAllWorkers()

  def killAllWorkers(): Unit = context.children.foreach(context.stop)

  def needNewCandidate(b: EncryBlock): Boolean =
    !candidateOpt.flatMap(_.parentOpt).map(_.id).exists(_.sameElements(b.header.id))

  def shouldStartMine(b: EncryBlock): Boolean =
    settings.node.mining && b.header.timestamp >= timeProvider.time() && context.children.nonEmpty

  def unknownMessage: Receive = {
    case m => logWarn(s"Unexpected message $m")
  }

  def mining: Receive = {

    case StartMining if context.children.nonEmpty =>
      candidateOpt match {
        case Some(candidateBlock) =>
          log.info(s"Start mining in ${dateFormat.format(new Date(System.currentTimeMillis()))}")
          context.children.foreach(_ ! NextChallenge(candidateBlock))
        case None => produceCandidate()
      }
    case StartMining =>
      val numberOfWorkers: Int = settings.node.numberOfMiningWorkers
      for (i <- 0 until numberOfWorkers) yield context.actorOf(
        Props(classOf[EncryMiningWorker], i, numberOfWorkers).withDispatcher("mining-dispatcher"), s"worker$i")
      self ! StartMining
    case DisableMining if context.children.nonEmpty =>
      killAllWorkers()
      context.become(miningDisabled)
    case MinedBlock(block, workerIdx) if candidateOpt.exists(_.stateRoot sameElements block.header.stateRoot) =>
      nodeViewHolder ! LocallyGeneratedModifier(block.header)
      nodeViewHolder ! LocallyGeneratedModifier(block.payload)
      if (settings.node.sendStat) {
        system.actorSelection("user/statsSender") ! MiningEnd(block.header, workerIdx, context.children.size)
        system.actorSelection("user/statsSender") ! MiningTime(System.currentTimeMillis() - startTime)
      }
      if (settings.node.stateMode == StateMode.Digest)
        block.adProofsOpt.foreach(adp => nodeViewHolder ! LocallyGeneratedModifier(adp))
      candidateOpt = None
      sleepTime = System.currentTimeMillis()
      context.children.foreach(_ ! DropChallenge)
    case GetMinerStatus => sender ! MinerStatus(context.children.nonEmpty, candidateOpt)
    case _ =>
  }

  def miningDisabled: Receive = {
    case EnableMining =>
      context.become(miningEnabled)
      self ! StartMining
    case GetMinerStatus => sender ! MinerStatus(context.children.nonEmpty, candidateOpt)
  }

  def receiveSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(mod: EncryBlock) if context.children.nonEmpty && needNewCandidate(mod) =>
      log.info(s"Got new block. Starting to produce candidate on height: ${mod.header.height + 1} " +
        s"in ${dateFormat.format(new Date(System.currentTimeMillis()))}")
      produceCandidate()
    case SemanticallySuccessfulModifier(mod: EncryBlock) if shouldStartMine(mod) =>
      log.info(s"Got new block2. Starting to produce candidate on height: ${mod.header.height + 1} " +
        s"in ${dateFormat.format(new Date(System.currentTimeMillis()))}")
      self ! StartMining
    case SemanticallySuccessfulModifier(_) =>
  }

  def receiverCandidateBlock: Receive = {
    case c: CandidateBlock => procCandidateBlock(c)
    case cEnv: CandidateEnvelope if cEnv.c.nonEmpty => procCandidateBlock(cEnv.c.get)
  }

  override def receive: Receive = if (settings.node.mining) miningEnabled else miningDisabled

  def miningEnabled: Receive =
    receiveSemanticallySuccessfulModifier orElse
      receiverCandidateBlock orElse
      mining orElse
      unknownMessage

  def procCandidateBlock(c: CandidateBlock): Unit = {
    log.info(s"Got candidate block $c in ${dateFormat.format(new Date(System.currentTimeMillis()))}")
    candidateOpt = Some(c)
    context.system.scheduler.scheduleOnce(settings.node.miningDelay, self, StartMining)
  }

  def createCandidate(view: CurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool],
                      bestHeaderOpt: Option[EncryBlockHeader]): CandidateBlock = {
    val timestamp: Time = timeProvider.time()
    val height: Height = Height @@ (bestHeaderOpt.map(_.height).getOrElse(Constants.Chain.PreGenesisHeight) + 1)

    // `txsToPut` - valid, non-conflicting txs with respect to their fee amount.
    // `txsToDrop` - invalidated txs to be dropped from mempool.
    val (txsToPut: Seq[BaseTransaction], txsToDrop: Seq[BaseTransaction], _) = view.pool.takeAll.toSeq.sortBy(_.fee).reverse
      .foldLeft((Seq[BaseTransaction](), Seq[BaseTransaction](), Set[ByteArrayWrapper]())) {
        case ((validTxs, invalidTxs, bxsAcc), tx) =>
          val bxsRaw: IndexedSeq[ByteArrayWrapper] = tx.inputs.map(u => ByteArrayWrapper(u.boxId))
          if ((validTxs.map(_.length).sum + tx.length) <= Constants.BlockMaxSize - 124) {
            if (view.state.validate(tx).isSuccess && bxsRaw.forall(k =>
              !bxsAcc.contains(k)) && bxsRaw.size == bxsRaw.toSet.size)
              (validTxs :+ tx, invalidTxs, bxsAcc ++ bxsRaw)
            else (validTxs, invalidTxs :+ tx, bxsAcc)
          } else (validTxs, invalidTxs, bxsAcc)
      }
    // Remove stateful-invalid txs from mempool.
    view.pool.removeAsync(txsToDrop)

    val minerSecret: PrivateKey25519 = view.vault.keyManager.mainKey
    val feesTotal: Amount = txsToPut.map(_.fee).sum
    val supplyTotal: Amount = EncrySupplyController.supplyAt(view.state.height)
    val coinbase: EncryTransaction = TransactionFactory
      .coinbaseTransactionScratch(minerSecret.publicImage, timestamp, supplyTotal, feesTotal, view.state.height)

    val txs: Seq[BaseTransaction] = txsToPut.sortBy(_.timestamp) :+ coinbase

    val (adProof: SerializedAdProof, adDigest: ADDigest) = view.state.generateProofs(txs)
      .getOrElse(throw new Exception("ADProof generation failed"))

    val difficulty: Difficulty = bestHeaderOpt.map(parent => view.history.requiredDifficultyAfter(parent))
      .getOrElse(Constants.Chain.InitialDifficulty)

    val candidate: CandidateBlock =
      CandidateBlock(bestHeaderOpt, adProof, adDigest, Constants.Chain.Version, txs, timestamp, difficulty)

    log.info(s"Sending candidate block with ${candidate.transactions.length - 1} transactions " +
      s"and 1 coinbase for height $height")

    candidate
  }

  def produceCandidate(): Unit =
    nodeViewHolder ! GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, CandidateEnvelope] { view =>
      log.info(s"Starting candidate generation in ${dateFormat.format(new Date(System.currentTimeMillis()))}")
      startTime = System.currentTimeMillis()
      if (settings.node.sendStat) {
        system.actorSelection("user/statsSender") ! SleepTime(System.currentTimeMillis() - sleepTime)
      }
      val bestHeaderOpt: Option[EncryBlockHeader] = view.history.bestBlockOpt.map(_.header)
      val candidate = if (bestHeaderOpt.isDefined || settings.node.offlineGeneration)
        CandidateEnvelope.fromCandidate(createCandidate(view, bestHeaderOpt))
      else CandidateEnvelope.empty
      if (settings.node.sendStat) {
        system.actorSelection("user/statsSender") ! CandidateProducingTime(System.currentTimeMillis() - startTime)
      }
      candidate
    }
}

object EncryMiner extends Logging {

  case object DisableMining

  case object EnableMining

  case object StartMining

  case object GetMinerStatus

  case class MinedBlock(block: EncryBlock, workerIdx: Int)

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