package encry.local.miner

import java.util.Date

import akka.actor.{Actor, ActorRef}
import encry.EncryApp._

import scala.concurrent.duration._
import encry.consensus.{CandidateBlock, ConsensusSchemeReaders}
import encry.local.miner.Miner.MinedBlock
import encry.local.miner.Worker.{MineBlock, NextChallenge}
import java.text.SimpleDateFormat

import com.typesafe.scalalogging.StrictLogging
import org.encryfoundation.common.utils.constants.TestNetConstants

class Worker(myIdx: Int, numberOfWorkers: Int, miner: ActorRef) extends Actor with StrictLogging {

  val sdf: SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")
  var challengeStartTime: Date = new Date(System.currentTimeMillis())

  val initialNonce: Long = Long.MaxValue / numberOfWorkers * myIdx

  override def preRestart(reason: Throwable, message: Option[Any]): Unit =
    logger.warn(s"Worker $myIdx is restarting because of: $reason")

  override def receive: Receive = {
    case MineBlock(candidate: CandidateBlock, nonce: Long) =>
      logger.info(s"Trying nonce: $nonce. Start nonce is: $initialNonce. " +
        s"Iter qty: ${nonce - initialNonce + 1} on worker: $myIdx with diff: ${candidate.difficulty}")
      ConsensusSchemeReaders
        .consensusScheme.verifyCandidate(candidate, nonce)
        .fold(
          e => {
            self ! MineBlock(candidate, nonce + 1)
            logger.info(s"Mining failed cause: $e")
          },
          block => {
            logger.info(s"New block is found: (${block.header.height}, ${block.header.encodedId}, ${block.payload.txs.size} " +
              s"on worker $self at ${sdf.format(new Date(System.currentTimeMillis()))}. Iter qty: ${nonce - initialNonce + 1}")
            miner ! MinedBlock(block, myIdx)
          })
    case NextChallenge(candidate: CandidateBlock) =>
      challengeStartTime = new Date(System.currentTimeMillis())
      logger.info(s"Start next challenge on worker: $myIdx at height " +
        s"${candidate.parentOpt.map(_.height + 1).getOrElse(TestNetConstants.PreGenesisHeight.toString)} at ${sdf.format(challengeStartTime)}")
      self ! MineBlock(candidate, Long.MaxValue / numberOfWorkers * myIdx)
  }

}

object Worker {

  case class NextChallenge(candidateBlock: CandidateBlock)

  case class MineBlock(candidateBlock: CandidateBlock, nonce: Long)

}