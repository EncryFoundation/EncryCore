package encry.consensus

import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId}
import scala.math.BigInt
import scorex.crypto.hash.Digest32

trait ConsensusScheme {

  def verifyCandidate(candidateBlock: CandidateBlock, nonce: Long): Either[String, Block]

  def realDifficulty(header: Header): BigInt

  def getDerivedHeaderFields(parentOpt: Option[Header],
                             transactions: Seq[Transaction]): (Byte, ModifierId, Digest32, Int)

  def correctWorkDone(realDifficulty: Difficulty, difficulty: BigInt): Boolean = realDifficulty >= difficulty
}