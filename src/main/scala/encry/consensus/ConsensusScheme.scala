package encry.consensus

import encry.crypto.equihash.EquihashValidationErrors
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.utils.TaggedTypes.Difficulty
import scala.math.BigInt

trait ConsensusScheme {

  def verifyCandidate(candidateBlock: CandidateBlock, nonce: Long): Either[EquihashValidationErrors, Block]

  def realDifficulty(header: Header): BigInt

  def correctWorkDone(realDifficulty: Difficulty, difficulty: BigInt): Boolean = realDifficulty >= difficulty
}