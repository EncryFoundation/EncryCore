package encry.mining

import akka.actor.{Actor, ActorRef}
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.utils.Cancellable
import scorex.core.ModifierId
import scorex.core.block.Block.Version
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32

import scala.util.Random

class PowMiner(viewHolderRef: ActorRef, settings: Any) extends Actor with ScorexLogging {

  import PowMiner._

  private var cancellableOpt: Option[Cancellable] = None
  private var active = false

  override def receive: Receive = {

    // TODO: Tmp.
    case StartMining => self ! MineBlock

//    case StartMining =>
//      if (settings.blockGenerationDelay >= 1.minute) {
//        log.info("Mining is disabled for blockGenerationDelay >= 1 minute")
//      } else {
//        active = true
//        self ! MineBlock
//      }
//
//    case MineBlock =>
//      if (active) {
//        log.info("Mining of previous PoW block stopped")
//        cancellableOpt.forall(_.cancel())
//
//        context.system.scheduler.scheduleOnce(50.millis) {
//          if (cancellableOpt.forall(_.status.isCancelled)) viewHolderRef ! getRequiredData
//          else self ! StartMining
//        }
//      }
  }
}

object PowMiner extends App {

  case object StartMining

  case object StopMining

  case object MineBlock

  // Attempts to find the right `nonce` for block.
  def powIteration(version: Version,
                   parentId: ModifierId,
                   txMerkleRoot: Digest32,
                   height: Int,
                   difficulty: Int,
                   generatorProposition: PublicKey25519Proposition): Option[EncryBlockHeader] = {

    val nonce = Random.nextLong()
    val timestamp = System.currentTimeMillis()

    val block = EncryBlockHeader(
      version, parentId, txMerkleRoot, timestamp, height, nonce, difficulty, generatorProposition)

    println("Testing block hash: " + Base16.encode(block.id))

    val result = if (block.validPow) Some(block) else None

    result
  }
}
