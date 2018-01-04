package encry.mining

import akka.actor.{Actor, ActorRef}
import encry.consensus.Difficulty
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.ChainSettings
import encry.utils.Cancellable
import scorex.core.ModifierId
import scorex.core.block.Block.Version
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32

import scala.util.Random

class EncryMiner(viewHolderRef: ActorRef, chainSettings: ChainSettings, nodeId: Array[Byte])
  extends Actor with ScorexLogging {

  import EncryMiner._

  private var cancellableOpt: Option[Cancellable] = None
  private var isMining = false
  private var nonce = 0
  private var candidateOpt: Option[EncryBlockHeader] = None

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

object EncryMiner extends ScorexLogging {

  case object StartMining

  case object StopMining

  case object MineBlock

  // Makes one attempt to find the right `nonce` for block.
  def powIteration(version: Version,
                   parentId: ModifierId,
                   adProofRoot: Digest32,
                   stateRoot: ADDigest,
                   txMerkleRoot: Digest32,
                   height: Int,
                   difficulty: Difficulty,
                   generatorProposition: PublicKey25519Proposition): Option[EncryBlockHeader] = {

    val nonce = Random.nextLong()
    val timestamp = System.currentTimeMillis()

    val blockHeader = EncryBlockHeader(
      version, parentId, adProofRoot, stateRoot, txMerkleRoot, timestamp, height, nonce, difficulty, generatorProposition)

    println("Testing block hash: " + Base16.encode(blockHeader.id))

    // TODO: !!!
    val result = if (/*blockHeader.validPow*/ 1 == 1) Some(blockHeader) else None

    result
  }
}
