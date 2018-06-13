package encry.local.miner

import akka.actor.Actor
import encry.EncryApp._
import encry.consensus.{CandidateBlock, ConsensusSchemeReaders}
import encry.stats.StatsSender.MiningEnd
import encry.view.EncryNodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import encry.view.state.StateMode
import scorex.core.utils.ScorexLogging

class EncryMiningWorker(initialCandidate: CandidateBlock, myNumber: Int, numberOfWorkers: Int)
  extends Actor with ScorexLogging {

  case class MineBlock(nonce: Long)

  var candidate: CandidateBlock = initialCandidate

  override def preStart(): Unit = {
    log.info("Starting new mining worker: " + self.path)
    context.system.scheduler.scheduleOnce(settings.node.miningDelay) {
      self ! MineBlock(Long.MaxValue / numberOfWorkers * myNumber)
    }
  }

  override def receive: Receive = {
    case newCandidate: CandidateBlock => candidate = newCandidate
    case MineBlock(nonce) => ConsensusSchemeReaders.consensusScheme.verifyCandidate(candidate, nonce) match {
      case Some(block) =>
        log.info(s"New block is found: $block with on worker $self.")
        statsSender ! MiningEnd(block.header, myNumber)
        nodeViewHolder ! LocallyGeneratedModifier(block.header)
        nodeViewHolder ! LocallyGeneratedModifier(block.payload)
        if (settings.node.stateMode == StateMode.Digest)
          block.adProofsOpt.foreach { adp => nodeViewHolder ! LocallyGeneratedModifier(adp) }
        context.system.scheduler.scheduleOnce(settings.node.miningDelay) {
          self ! MineBlock(Long.MaxValue / numberOfWorkers * myNumber)
        }
      case None => self ! MineBlock(nonce + 1)
    }
  }
}
