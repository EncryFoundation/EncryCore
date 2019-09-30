package encry.local.miner

import java.text.SimpleDateFormat
import java.util.Date

import akka.actor.{Actor, ActorRef, Kill, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.{UpdatingMinerStatus, UpdatingTransactionsNumberForApi}
import encry.consensus.{CandidateBlock, EquihashPowScheme, SupplyController}
import encry.local.miner.Miner._
import encry.local.miner.Worker.NextChallenge
import encry.modifiers.mempool.TransactionFactory
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.EncryAppSettings
import encry.stats.StatsSender._
import encry.utils.NetworkTime.Time
import encry.view.actors.NodeViewHolder.ReceivableMessages.LocallyGeneratedBlock
import encry.view.mempool.MemoryPool.TransactionsForMiner
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height}
import encry.utils.NetworkTimeProvider
import encry.EncryApp.nodeViewHolder
import scala.collection._
import scala.concurrent.duration._
import SupplyController._
import cats.syntax.option._

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

  def killAllWorkers(): Unit = context.children.foreach(_ ! Kill)

  def needNewCandidate(b: Block): Boolean = !candidateOpt.flatMap(_.parentOpt.map(_.id)).exists(_ sameElements b.header.id)

  override def receive: Receive = if (settings.node.mining && isSyncingDone) miningEnabled else miningDisabled

  def mining: Receive = {
    case msg@StartMining if context.children.nonEmpty && isSyncingDone =>
      logger.info(s"Miner got StartMining but context.children.nonEmpty = ${context.children.nonEmpty}.")
      killAllWorkers(); self ! msg
    case StartMining if candidateOpt.nonEmpty && isSyncingDone =>
      logger.info(s"Miner got StartMining and candidateOpt = $candidateOpt && isSyncingDone = $isSyncingDone.")
      for (i <- 0 until settings.node.numberOfMiningWorkers) yield context.actorOf(Props(
        classOf[Worker], i, settings.node.numberOfMiningWorkers, settings.constants.PreGenesisHeight
      ).withDispatcher("mining-dispatcher").withMailbox("mining-mailbox"))
      logger.info(s"Starting mining at ${dateFormat.format(new Date(System.currentTimeMillis()))}.")
      candidateOpt.foreach(candidate => context.children.foreach(_ ! NextChallenge(candidate)))
    case StartMining if isSyncingDone =>
      logger.info(s"Miner got StartMining but candidateOpt = $candidateOpt. Create new candidate.")
      val coinbase: Transaction = TransactionFactory
        .coinbaseTransactionScratch(minerSecret.publicImage, timestamp, supplyTotal, feesTotal, currentHeight)
      nodeViewHolder ! StartProducingNewCandidate(transactionsPool, none, Difficulty @@ BigInt(0))
      transactionsPool = IndexedSeq.empty[Transaction] //tmp decision
    case StartMining => logger.info(s"Miner -> isSyncingDone = $isSyncingDone.")
    case NewCandidate(txs, header, difficulty, stateRoot, account) =>
      logger.info(s"\n\nMiner got NewCandidate. start processing new candidate. ${Algos.encode(stateRoot)}.\n\n")
      self ! createNewCandidateBlock(txs, header, account, difficulty, stateRoot)
    case WrongConditionsForNewBlock(txs) =>
      logger.info(s"Miner got wrong condition.")
      transactionsPool ++= txs; self ! CandidateEnvelope.empty
    case TransactionsForMiner(transactions) => transactionsPool ++= transactions
    case MinedBlock(block, workerIdx) if candidateOpt.exists(_.timestamp == block.header.timestamp) =>
      logger.info(s"Going to propagate new block " +
        s"\n (${block.header.height}, ${block.payload.encodedId} ${block.header.encodedId}, ${block.payload.txs.size}" +
        s" from worker $workerIdx with nonce: ${block.header.nonce}.")
      killAllWorkers()
      nodeViewHolder ! LocallyGeneratedBlock(block)
      candidateOpt = None
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
      nodeViewHolder ! StartProducingNewCandidate(transactionsPool, none, Difficulty @@ BigInt(0))
      transactionsPool = IndexedSeq.empty[Transaction] //tmp decisio
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

  def createNewCandidateBlock(validatedTxs: IndexedSeq[Transaction],
                              bestHeaderOpt: Option[Header],
                              minerSecret: PrivateKey25519,
                              difficulty: Difficulty,
                              stateRoot: Array[Byte]): CandidateEnvelope = {
    val filteredTxs: IndexedSeq[Transaction] = validatedTxs.foldLeft(List.empty[String], IndexedSeq.empty[Transaction]) {
      case ((usedInputsIds, acc), tx) =>
        if (tx.inputs.forall(input => !usedInputsIds.contains(Algos.encode(input.boxId))))
          (usedInputsIds ++ tx.inputs.map(input => Algos.encode(input.boxId))) -> (acc :+ tx)
        else usedInputsIds -> acc
    }._2
    val currentHeight: Height = Height @@ (bestHeaderOpt map (_.height) getOrElse settings.constants.PreGenesisHeight)
    val newHeight: Height = Height @@ (currentHeight + 1)
    val timestamp: Time = timeProvider.estimatedTime
    val feesTotal: Amount = filteredTxs.map(_.fee).sum
    val supplyTotal: Amount = supplyAt(currentHeight, settings.constants)
    val candidate: CandidateEnvelope =
      CandidateEnvelope.fromCandidate(
        CandidateBlock(bestHeaderOpt, settings.constants.Version, filteredTxs, timestamp, difficulty, stateRoot)
      )
    logger.info(s"$candidate at height $newHeight.")
    transactionsPool = IndexedSeq.empty[Transaction]
    candidate
  }
}

object Miner {

  final case class StartProducingNewCandidate(txs: IndexedSeq[Transaction],
                                              bestHeaderOpt: Option[Header],
                                              difficulty: Difficulty)

  final case class CandidateWithOutMandatoryAccount(validatedTxs: IndexedSeq[Transaction],
                                                    bestHeaderOpt: Option[Header],
                                                    difficulty: Difficulty,
                                                    stateRoot: Array[Byte])

  final case class NewCandidate(validatedTxs: IndexedSeq[Transaction],
                                bestHeaderOpt: Option[Header],
                                difficulty: Difficulty,
                                stateRoot: Array[Byte],
                                account: PrivateKey25519)

  final case class WrongConditionsForNewBlock(txs: IndexedSeq[Transaction]) extends AnyVal

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