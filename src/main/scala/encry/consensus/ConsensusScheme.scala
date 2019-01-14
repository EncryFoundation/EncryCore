package encry.consensus

import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.modifiers.history.{Block, Header}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.mempool.Transaction
import org.encryfoundation.common.utils.TaggedTypes.SerializedAdProof
import scorex.crypto.hash.Digest32

import scala.math.BigInt

trait ConsensusScheme {

  def verifyCandidate(candidateBlock: CandidateBlock, finishingNonce: Long, startingNonce: Long): Option[Block]

  def realDifficulty(header: Header): BigInt

  def getDerivedHeaderFields(parentOpt: Option[Header], adProofBytes: SerializedAdProof,
                             transactions: Seq[Transaction]): (Byte, ModifierId, Digest32, Digest32, Int)

  def correctWorkDone(realDifficulty: Difficulty, difficulty: BigInt): Boolean = {
    realDifficulty >= difficulty
  }
}