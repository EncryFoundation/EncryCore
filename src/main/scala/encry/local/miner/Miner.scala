package encry.local.miner

import java.text.SimpleDateFormat
import java.util.Date
import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.{UpdatingMinerStatus, UpdatingTransactionsNumberForApi}
import encry.consensus.{CandidateBlock, EquihashPowScheme, SupplyController}
import encry.local.miner.Miner._
import encry.local.miner.Worker.NextChallenge
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.EncryAppSettings
import encry.stats.StatsSender._
import encry.view.actors.NodeViewHolder.ReceivableMessages.LocallyGeneratedBlock
import encry.view.mempool.MemoryPool.TransactionsForMiner
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import encry.utils.NetworkTimeProvider
import encry.EncryApp.nodeViewHolder
import scala.collection._
import scala.concurrent.duration._
import encry.view.actors.NodeViewHolder.{InfoForCandidateIsReady, StartAggregatingInfoForCandidateBlock, WrongConditionsForCandidate}

class Miner(dataHolder: ActorRef,
            influx: Option[ActorRef],
            settings: EncryAppSettings,
            timeProvider: NetworkTimeProvider) extends Actor with StrictLogging {

  import context.dispatcher

  val dateFormat: SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")
  val powScheme: EquihashPowScheme = EquihashPowScheme(settings.constants)
  var candidateOpt: Option[CandidateBlock] = None
  var transactionsPool: IndexedSeq[Transaction] = IndexedSeq.empty[Transaction]
  var isSyncingDone: Boolean = settings.node.offlineGeneration

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])
    context.system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])
    context.system.scheduler.schedule(5.seconds, 5.seconds)(
      influx.foreach(_ ! InfoAboutTransactionsFromMiner(transactionsPool.size))
    )
    context.system.scheduler.schedule(5.seconds, 5.seconds) {
      dataHolder ! UpdatingTransactionsNumberForApi(transactionsPool.length)
      dataHolder ! UpdatingMinerStatus(MinerStatus(context.children.nonEmpty && candidateOpt.nonEmpty, candidateOpt))
    }
  }

  override def postStop(): Unit = killAllWorkers()

  def killAllWorkers(): Unit = context.children.foreach(context.stop)

  def needNewCandidate(b: Block): Boolean = !candidateOpt.flatMap(_.parentOpt.map(_.id)).exists(_ sameElements b.header.id)

  override def receive: Receive = if (settings.node.mining && isSyncingDone) miningEnabled else miningDisabled

  def mining: Receive = {
    case WrongConditionsForCandidate(txs) =>
      logger.info(s"Miner got message WrongConditionsForCandidate.")
      transactionsPool ++= txs
      self ! CandidateEnvelope.empty
    case InfoForCandidateIsReady(header, txs, timestamp, difficulty, stateRoot) =>
      logger.info(s"Got InfoForCandidateIsReady on miner")
      self ! CandidateEnvelope.fromCandidate(
        CandidateBlock(header, settings.constants.Version, txs, timestamp, difficulty, stateRoot)
      )
    case msg@StartMining if context.children.nonEmpty && isSyncingDone =>
      killAllWorkers()
      self ! msg
    case StartMining if candidateOpt.nonEmpty && isSyncingDone =>
      logger.info(s"Miner got StartMining and candidateOpt = $candidateOpt && isSyncingDone = $isSyncingDone.")
      for (i <- 0 until settings.node.numberOfMiningWorkers) yield context.actorOf(Props(
        classOf[Worker], i, settings.node.numberOfMiningWorkers, settings.constants.PreGenesisHeight
      ).withDispatcher("mining-dispatcher").withMailbox("mining-mailbox"))
      logger.info(s"Starting mining at ${dateFormat.format(new Date(System.currentTimeMillis()))}.")
      candidateOpt.foreach(candidate => context.children.foreach(_ ! NextChallenge(candidate)))
    case StartMining if isSyncingDone =>
      logger.info(s"Miner got StartMining but candidateOpt = $candidateOpt. Create new candidate.")
      nodeViewHolder ! StartAggregatingInfoForCandidateBlock(transactionsPool)
      transactionsPool = IndexedSeq.empty[Transaction] //tmp decision
    case StartMining => logger.info(s"Miner -> isSyncingDone = $isSyncingDone.")
    case TransactionsForMiner(transactions) => transactionsPool ++= transactions
    case MinedBlock(block, workerIdx) if candidateOpt.exists(_.stateRoot sameElements block.header.stateRoot) =>
      logger.info(s"Going to propagate new block " +
        s"\n (${block.header.height}, ${block.payload.encodedId} ${block.header.encodedId}, ${block.payload.txs.size}" +
        s" from worker $workerIdx with nonce: ${block.header.nonce}.")
      killAllWorkers()
      nodeViewHolder ! LocallyGeneratedBlock(block)
      candidateOpt = None
    case MinedBlock(_, workerIdx) => logger.info(s"get mined block from worker ${workerIdx}")
    case DisableMining if context.children.nonEmpty =>
      killAllWorkers()
      candidateOpt = None
      context.become(miningDisabled)
  }

  def miningEnabled: Receive = receiveSemanticallySuccessfulModifier
    .orElse(receiverCandidateBlock)
    .orElse(mining)
    .orElse(chainEvents)
    .orElse(unknownMessage)

  def miningDisabled: Receive = {
    case EnableMining =>
      context.become(miningEnabled)
      self ! StartMining
    case FullBlockChainIsSynced() =>
      isSyncingDone = true
      if (settings.node.mining) self ! EnableMining
    case TransactionsForMiner(_) =>
    case DisableMining | SemanticallySuccessfulModifier(_) =>
  }

  def receiveSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(mod: Block) if needNewCandidate(mod) =>
      logger.info(s"Got new block. Starting to produce candidate at height: ${mod.header.height + 1} " +
        s"at ${dateFormat.format(new Date(System.currentTimeMillis()))}")
      nodeViewHolder ! StartAggregatingInfoForCandidateBlock(transactionsPool)
      transactionsPool = IndexedSeq.empty[Transaction]
    case SemanticallySuccessfulModifier(_) =>
  }

  def receiverCandidateBlock: Receive = {
    case c: CandidateBlock =>
      logger.info(s"Got new candidate $c")
      procCandidateBlock(c)
    case cEnv: CandidateEnvelope if cEnv.c.nonEmpty => procCandidateBlock(cEnv.c.get)
    case _: CandidateEnvelope =>
      logger.debug("Received empty CandidateEnvelope, going to suspend mining for a while")
      self ! DisableMining
  }

  def unknownMessage: Receive = {
    case m => logger.debug(s"Unexpected message $m")
  }

  def chainEvents: Receive = {
    case FullBlockChainIsSynced() => isSyncingDone = true
  }

  def procCandidateBlock(c: CandidateBlock): Unit = {
    logger.info(s"Got candidate block $c in ${dateFormat.format(new Date(System.currentTimeMillis()))}")
    candidateOpt = Some(c)
    self ! StartMining
  }
}

object Miner {

  final case class CandidateEnvelope(c: Option[CandidateBlock]) extends AnyVal

  final case class MinedBlock(block: Block, workerIdx: Int)

  case object DisableMining

  case object EnableMining

  case object StartMining


  object CandidateEnvelope {

    val empty: CandidateEnvelope = CandidateEnvelope(None)

    def fromCandidate(c: CandidateBlock): CandidateEnvelope = CandidateEnvelope(Some(c))
  }

  case class MinerStatus(isMining: Boolean, candidateBlock: Option[CandidateBlock]) {
    lazy val json: Json = Map(
      "isMining"       -> isMining.asJson,
      "candidateBlock" -> candidateBlock.map(_.asJson).getOrElse("None".asJson)
    ).asJson
  }

  implicit val jsonEncoder: Encoder[MinerStatus] = (r: MinerStatus) => Map(
    "isMining"       -> r.isMining.asJson,
    "candidateBlock" -> r.candidateBlock.map(_.asJson).getOrElse("None".asJson)
  ).asJson

  def props(dataHolder: ActorRef,
            influx: Option[ActorRef],
            settings: EncryAppSettings,
            timeProvider: NetworkTimeProvider): Props = Props(new Miner(dataHolder, influx, settings, timeProvider))
}