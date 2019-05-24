package encry.consensus

import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId, SerializedAdProof}
import scorex.crypto.hash.Digest32

import scala.math.BigInt

trait ConsensusScheme {

  def verifyCandidate(candidateBlock: CandidateBlock, nonce: Long): Option[Block] =
    verifyCandidate(candidateBlock, nonce, nonce)

  def verifyCandidate(candidateBlock: CandidateBlock, finishingNonce: Long, startingNonce: Long): Option[Block]

  def realDifficulty(header: Header): BigInt

  def getDerivedHeaderFields(parentOpt: Option[Header], adProofBytes: SerializedAdProof,
                             transactions: Seq[Transaction]): (Byte, ModifierId, Digest32, Digest32, Int)

  def correctWorkDone(realDifficulty: Difficulty, difficulty: BigInt): Boolean = {
    realDifficulty >= difficulty
  }
}