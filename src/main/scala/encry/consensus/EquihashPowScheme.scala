package encry.consensus

import com.google.common.primitives.Chars
import encry.crypto.equihash.{Equihash, EquihashValidationErrors}
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height, ModifierId}
import scorex.crypto.hash.Digest32
import scala.math.BigInt
import cats.syntax.either._
import encry.crypto.equihash.EquihashValidationErrors._

case class EquihashPowScheme(n: Char, k: Char, version: Byte, preGenesisHeight: Height, maxTarget: BigInt)
  extends ConsensusScheme {

  private val seed: Array[Byte] =
    "equi_seed_12".getBytes(Algos.charset) ++ Chars.toByteArray(n) ++ Chars.toByteArray(k)

  override def verifyCandidate(candidateBlock: CandidateBlock,
                               startingNonce: Long): Either[EquihashValidationErrors, Block] = {
    val difficulty = candidateBlock.difficulty
    val parentId: ModifierId = candidateBlock.parentOpt.map(_.id).getOrElse(Header.GenesisParentId)
    val txsRoot: Digest32 = Payload.rootHash(candidateBlock.transactions.map(_.id))
    val height: Int = candidateBlock.parentOpt.map(_.height).getOrElse(preGenesisHeight) + 1
    val bytesPerWord: Int = n / 8
    val wordsPerHash: Int = 512 / n
    val digest: Blake2bDigest = new Blake2bDigest(null, bytesPerWord * wordsPerHash, null, seed)
    val header: Header = Header(
      version, parentId, txsRoot,
      candidateBlock.timestamp, height, 0L, candidateBlock.difficulty, EquihashSolution.empty, candidateBlock.stateRoot
    )
    for {
      possibleHeader <- generateHeader(startingNonce, digest, header, difficulty)
      validationResult = verify(possibleHeader)
      _ <- Either.cond(validationResult.isRight, (),
        VerifyingCandidateValidationError(s"Incorrect possible header cause: $validationResult"))
      payload: Payload = Payload(possibleHeader.id, candidateBlock.transactions)
    } yield Block(possibleHeader, payload)
  }

  private def generateHeader(nonce: Long,
                             digest: Blake2bDigest,
                             header: Header,
                             difficulty: Difficulty): Either[EquihashValidationErrors, Header] = {
    val currentDigest = new Blake2bDigest(digest)
    Equihash.hashNonce(currentDigest, nonce)
    val solutions = Equihash.gbpBasic(currentDigest, n, k)
    solutions
      .map(solution => header.copy(nonce = nonce, equihashSolution = solution))
      .find(newHeader => correctWorkDone(realDifficulty(newHeader), difficulty))
    match {
      case Some(value) => value.asRight[EquihashValidationErrors]
      case None => GeneratingHeaderError("Generate header failed").asLeft[Header]
    }
  }

  def verify(header: Header): Either[EquihashValidationErrors, Boolean] = Equihash
    .validateSolution(n, k, seed, Equihash.nonceToLeBytes(header.nonce), header.equihashSolution.indexedSeq)

  override def realDifficulty(header: Header): Difficulty =
    Difficulty @@ (maxTarget / BigInt(1, header.powHash))

  override def toString: String = s"EquihashPowScheme(n = ${n.toInt}, k = ${k.toInt})"
}