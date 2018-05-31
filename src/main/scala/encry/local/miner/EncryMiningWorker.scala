package encry.local.miner

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import encry.consensus.{CandidateBlock, ConsensusSchemeReaders}
import encry.local.miner.EncryMiningWorker.MineBlock
import encry.settings.EncryAppSettings
import encry.view.state.StateMode
import encry.view.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class EncryMiningWorker(settings: EncryAppSettings,
                        viewHolderRef: ActorRef,
                        initialCandidate: CandidateBlock) extends Actor with ScorexLogging {


  private val consensus = ConsensusSchemeReaders.consensusScheme

  private var candidate: CandidateBlock = initialCandidate

  override def preStart(): Unit = {
    log.info("Booting new mining worker")
    context.system.scheduler.scheduleOnce(settings.nodeSettings.miningDelay) (self ! MineBlock(Random.nextLong()))
  }

  override def receive: Receive = {
    case newCandidate: CandidateBlock =>
      candidate = newCandidate

    case MineBlock(nonce) =>
      consensus.verifyCandidate(candidate, nonce) match {
        case Some(block) =>
          log.info(s"New block found: $block")

          viewHolderRef ! LocallyGeneratedModifier(block.header)
          viewHolderRef ! LocallyGeneratedModifier(block.payload)
          if (settings.nodeSettings.stateMode == StateMode.Digest) {
            block.adProofsOpt.foreach { adp =>
              viewHolderRef ! LocallyGeneratedModifier(adp)
            }
          }
          context.system.scheduler.scheduleOnce(settings.nodeSettings.miningDelay) {
            self ! MineBlock(Random.nextLong())
          }
        case None =>
          self ! MineBlock(nonce + 1)
      }
  }
}

object EncryMiningWorker {

  case class MineBlock(nonce: Long)

  def props(settings: EncryAppSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock): Props =
    Props(new EncryMiningWorker(settings, viewHolderRef, startCandidate))

  def apply(settings: EncryAppSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(settings, viewHolderRef, startCandidate))

  def apply(settings: EncryAppSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(settings, viewHolderRef, startCandidate), name)
}
