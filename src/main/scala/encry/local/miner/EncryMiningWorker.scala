package encry.local.miner

import java.util.Date

import akka.actor.Actor
import encry.EncryApp.miner
import encry.consensus.{CandidateBlock, ConsensusSchemeReaders}
import encry.local.miner.EncryMiner.MinedBlock
import encry.local.miner.EncryMiningWorker.{DropChallenge, MineBlock, NextChallenge}
import encry.utils.Logging
import java.text.SimpleDateFormat

import encry.settings.Constants

class EncryMiningWorker(myIdx: Int, numberOfWorkers: Int) extends Actor with Logging {

  val sdf: SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")
  var challengeStartTime: Date = new Date(System.currentTimeMillis())

  override def receive: Receive = miningPaused

  def miningInProgress: Receive = {
    case MineBlock(candidate: CandidateBlock, nonce: Long) =>
      val initialNonce: Long = Long.MaxValue / numberOfWorkers * myIdx
      log.info(s"Trying nonce: $nonce. Start nonce is: $initialNonce. " +
        s"Iter qty: ${nonce - initialNonce + 1} on worker: $myIdx with diff: ${candidate.difficulty}")
      ConsensusSchemeReaders.consensusScheme.verifyCandidate(candidate, nonce)
        .fold(self ! MineBlock(candidate, nonce + 1)) { block =>
          log.info(s"New block is found: $block on worker $self at " +
            s"${sdf.format(new Date(System.currentTimeMillis()))}. Iter qty: ${nonce - initialNonce + 1}")
          miner ! MinedBlock(block, myIdx)
        }
    case DropChallenge => context.become(miningPaused)
  }

  def miningPaused: Receive = {
    case NextChallenge(candidate: CandidateBlock) =>
      challengeStartTime = new Date(System.currentTimeMillis())
      context.become(miningInProgress)
      log.info(s"Start challenge on worker: $myIdx at height " +
        s"${candidate.parentOpt.map(_.height + 1).getOrElse(Constants.Chain.PreGenesisHeight.toString)} at ${sdf.format(challengeStartTime)}")
      self ! MineBlock(candidate, Long.MaxValue / numberOfWorkers * myIdx)
    case _ =>
  }
}

object EncryMiningWorker {

  case object DropChallenge

  case class NextChallenge(candidateBlock: CandidateBlock)

  case class MineBlock(candidateBlock: CandidateBlock, nonce: Long)

}
