package encry.local.miner

import akka.actor.{Actor, ActorRef}
import encry.consensus.{CandidateBlock, ConsensusSchemeReaders}
import encry.local.miner.EncryMiner.MinedBlock
import encry.local.miner.EncryMiningWorker.{DropChallenge, MineBlock, NextChallenge}
import encry.utils.ScorexLogging

class EncryMiningWorker(miner: ActorRef, myNumber: Int, numberOfWorkers: Int) extends Actor with ScorexLogging {

  override def receive: Receive = {

    case NextChallenge(candidate: CandidateBlock) => self ! MineBlock(candidate, Long.MaxValue / numberOfWorkers * myNumber)

    case MineBlock(candidate: CandidateBlock, nonce: Long) => ConsensusSchemeReaders.consensusScheme.verifyCandidate(candidate, nonce) match {
      case Some(block) =>
        log.info(s"New block is found: $block on worker $self.")
        //statsSender ! MiningEnd(block.header, myNumber)
        miner ! MinedBlock(block)
      case None => self ! MineBlock(candidate, nonce + 1)
    }

    case DropChallenge =>
  }
}

object EncryMiningWorker {

  case object DropChallenge

  case class NextChallenge(candidateBlock: CandidateBlock)

  case class MineBlock(candidateBlock: CandidateBlock, nonce: Long)
}
