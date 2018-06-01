package encry.local.miner

import akka.actor.Actor
import encry.consensus.{CandidateBlock, ConsensusScheme, ConsensusSchemeReaders}
import encry.view.state.StateMode
import encry.view.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import encry.EncryApp._
import scorex.core.utils.ScorexLogging

import scala.util.Random

class EncryMiningWorker(initialCandidate: CandidateBlock) extends Actor with ScorexLogging {

  case class MineBlock(nonce: Long)

  val consensus: ConsensusScheme = ConsensusSchemeReaders.consensusScheme

  var candidate: CandidateBlock = initialCandidate

  override def preStart(): Unit = {
    log.info("Starting new mining worker: " + self.path)
    context.system.scheduler.scheduleOnce(encrySettings.nodeSettings.miningDelay)(self ! MineBlock(Random.nextLong()))
  }

  override def receive: Receive = {
    case newCandidate: CandidateBlock => candidate = newCandidate
    case MineBlock(nonce) => consensus.verifyCandidate(candidate, nonce) match {
      case Some(block) =>
        log.info(s"New block found: $block")
        nodeViewHolder ! LocallyGeneratedModifier(block.header)
        nodeViewHolder ! LocallyGeneratedModifier(block.payload)
        if (encrySettings.nodeSettings.stateMode == StateMode.Digest)
          block.adProofsOpt.foreach { adp => nodeViewHolder ! LocallyGeneratedModifier(adp) }
        context.system.scheduler.scheduleOnce(encrySettings.nodeSettings.miningDelay) { self ! MineBlock(Random.nextLong()) }
      case None => self ! MineBlock(nonce + 1)
    }
  }
}
