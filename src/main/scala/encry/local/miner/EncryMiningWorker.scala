package encry.local.miner

import java.util.Date
import akka.actor.Actor
import encry.EncryApp.{miner, settings}
import encry.consensus.{CandidateBlock, ConsensusSchemeReaders}
import encry.local.miner.Miner.MinedBlock
import encry.local.miner.EncryMiningWorker.{MineBlock, NextChallenge}
import java.text.SimpleDateFormat
import encry.settings.Constants
import encry.stats.LoggingActor.LogMessage

class EncryMiningWorker(myIdx: Int, numberOfWorkers: Int) extends Actor {

  val sdf: SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")
  var challengeStartTime: Date = new Date(System.currentTimeMillis())

  val initialNonce: Long = Long.MaxValue / numberOfWorkers * myIdx

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = if (settings.logging.enableLogging)
    context.system.actorSelection("/user/loggingActor") !
      LogMessage("Warn", s"Worker $myIdx is restarting because of: $reason", System.currentTimeMillis())

  override def receive: Receive = {
    case MineBlock(candidate: CandidateBlock, nonce: Long) =>
      if (settings.logging.enableLogging)
        context.system.actorSelection("/user/loggingActor") ! LogMessage("Info", s"Trying nonce: $nonce. Start nonce is: $initialNonce. " +
          s"Iter qty: ${nonce - initialNonce + 1} on worker: $myIdx with diff: ${candidate.difficulty}", System.currentTimeMillis())
      ConsensusSchemeReaders.consensusScheme.verifyCandidate(candidate, nonce)
        .fold(self ! MineBlock(candidate, nonce + 1)) { block =>
          if (settings.logging.enableLogging)
            context.system.actorSelection("/user/loggingActor") ! LogMessage("Info", s"New block is found: $block on worker $self at " +
              s"${sdf.format(new Date(System.currentTimeMillis()))}. Iter qty: ${nonce - initialNonce + 1}", System.currentTimeMillis())
          miner ! MinedBlock(block, myIdx)
        }
    case NextChallenge(candidate: CandidateBlock) =>
      challengeStartTime = new Date(System.currentTimeMillis())
      if (settings.logging.enableLogging)
        context.system.actorSelection("/user/loggingActor") ! LogMessage("Info", s"Start next challenge on worker: $myIdx at height " +
          s"${candidate.parentOpt.map(_.height + 1).getOrElse(Constants.Chain.PreGenesisHeight.toString)} at ${sdf.format(challengeStartTime)}", System.currentTimeMillis())
      self ! MineBlock(candidate, Long.MaxValue / numberOfWorkers * myIdx)
  }

}

object EncryMiningWorker {

  case class NextChallenge(candidateBlock: CandidateBlock)

  case class MineBlock(candidateBlock: CandidateBlock, nonce: Long)

}
