package encry.local.miner

import java.util.Date

import akka.actor.Actor
import encry.EncryApp._
import encry.consensus.{CandidateBlock, ConsensusSchemeReaders}
import encry.local.miner.Miner.MinedBlock
import encry.local.miner.Worker.{MineBlock, NextChallenge, Status}
import java.text.SimpleDateFormat

import com.typesafe.scalalogging.StrictLogging
import encry.settings.Constants

import scala.concurrent.duration._

class Worker(myIdx: Int, numberOfWorkers: Int) extends Actor with StrictLogging {

  val sdf: SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")
  var challengeStartTime: Date = new Date(System.currentTimeMillis())
  var selfMsgTime: Long = 0L
  val initialNonce: Long = Long.MaxValue / numberOfWorkers * myIdx

  override def preStart(): Unit =
    context.system.scheduler.schedule(5.second, 5.second){
      logger.info(s"Worker $myIdx alive")
      self ! Status
    }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit =
    logger.warn(s"Worker $myIdx is restarting because of: $reason")

  override def receive: Receive = {
    case MineBlock(candidate: CandidateBlock, nonce: Long) =>
      if (selfMsgTime != 0L) logger.info(s"Time in mailbox: ${(System.currentTimeMillis() - selfMsgTime)/1000} seconds")
      logger.info(s"Trying nonce: $nonce. Start nonce is: $initialNonce. " +
        s"Iter qty: ${nonce - initialNonce + 1} on worker: $myIdx with diff: ${candidate.difficulty}")
      var verRes = ConsensusSchemeReaders.consensusScheme.verifyCandidate(candidate, initialNonce)
      var nonceToTest = nonce
      do {
        nonceToTest += 1
        verRes = ConsensusSchemeReaders.consensusScheme.verifyCandidate(candidate, nonceToTest)
      } while (verRes.isEmpty)
      logger.info(s"New block is found: ${verRes.get} on worker $self at " +
        s"${sdf.format(new Date(System.currentTimeMillis()))}. Iter qty: ${nonce - initialNonce + 1}")
      miner ! MinedBlock(verRes.get, myIdx)
    case NextChallenge(candidate: CandidateBlock) =>
      challengeStartTime = new Date(System.currentTimeMillis())
      logger.info(s"Start next challenge on worker: $myIdx at height " +
        s"${candidate.parentOpt.map(_.height + 1).getOrElse(Constants.Chain.PreGenesisHeight.toString)} at ${sdf.format(challengeStartTime)}")
        self ! MineBlock(candidate, Long.MaxValue / numberOfWorkers * myIdx)
    case Status => logger.info(s"Worker $myIdx sleep!")
  }

}

object Worker {

  case class NextChallenge(candidateBlock: CandidateBlock)

  case class MineBlock(candidateBlock: CandidateBlock, nonce: Long)

  case object Status
}
