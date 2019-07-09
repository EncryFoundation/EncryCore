package encry.consensus

import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.utils.TaggedTypes.Difficulty
import scala.math.BigInt

trait ConsensusScheme {

  def verifyCandidate(candidateBlock: CandidateBlock, nonce: Long): Either[String, Block] =
    verifyCandidate(candidateBlock, nonce, nonce)

  def verifyCandidate(candidateBlock: CandidateBlock, finishingNonce: Long, startingNonce: Long): Either[String, Block]

  def realDifficulty(header: Header): BigInt

  def correctWorkDone(realDifficulty: Difficulty, difficulty: BigInt): Boolean = realDifficulty >= difficulty
}