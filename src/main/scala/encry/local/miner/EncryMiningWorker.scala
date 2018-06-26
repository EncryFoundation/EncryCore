package encry.local.miner

import akka.actor.{Actor, ActorRef}
import encry.consensus.{CandidateBlock, ConsensusSchemeReaders}
import encry.local.miner.EncryMiner.MinedBlock
import encry.local.miner.EncryMiningWorker.{DropChallenge, MineBlock, NextChallenge}
import encry.utils.ScorexLogging

class EncryMiningWorker(miner: ActorRef, myNumber: Int, numberOfWorkers: Int) extends Actor with ScorexLogging {

  override def receive: Receive = miningPaused

  def miningInProgress: Receive = {

    case MineBlock(candidate: CandidateBlock, nonce: Long) => ConsensusSchemeReaders.consensusScheme.verifyCandidate(candidate, nonce)
      .fold(self ! MineBlock(candidate, nonce + 1)) { block =>
        log.info(s"New block is found: $block on worker $self.")
        miner ! MinedBlock(block)
      }

    case DropChallenge => context.become(miningPaused)
  }

  def miningPaused: Receive = {

    case NextChallenge(candidate: CandidateBlock) =>
      context.become(miningInProgress)
      self ! MineBlock(candidate, Long.MaxValue / numberOfWorkers * myNumber)

    case _ =>
  }
}

object EncryMiningWorker {

  case object DropChallenge

  case class NextChallenge(candidateBlock: CandidateBlock)

  case class MineBlock(candidateBlock: CandidateBlock, nonce: Long)
}
