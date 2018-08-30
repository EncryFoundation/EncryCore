package encry.consensus

import com.google.common.primitives.Chars
import encry.CoreTaggedTypes.ModifierId
import encry.crypto.equihash.{Equihash, EquihashSolution}
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.history.block.Block.Version
import encry.modifiers.mempool.Transaction
import encry.settings.Constants
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.SerializedAdProof
import scorex.crypto.hash.Digest32
import scala.annotation.tailrec
import scala.math.BigInt

case class EquihashPowScheme(n: Char, k: Char) extends ConsensusScheme {

  private val seed: Array[Byte] =
    "equi_seed_12".getBytes(Algos.charset) ++ Chars.toByteArray(n) ++ Chars.toByteArray(k)

  override def verifyCandidate(candidateBlock: CandidateBlock,
                               finishingNonce: Long,
                               startingNonce: Long): Option[EncryBlock] = {
    require(finishingNonce >= startingNonce)

    val difficulty: Difficulty = candidateBlock.difficulty

    val (version, parentId, adProofsRoot, txsRoot, height) =
      getDerivedHeaderFields(candidateBlock.parentOpt, candidateBlock.adProofBytes, candidateBlock.transactions)

    val bytesPerWord = n / 8
    val wordsPerHash = 512 / n

    val digest = new Blake2bDigest(null, bytesPerWord * wordsPerHash, null, seed) // scalastyle:ignore
    val h = EncryBlockHeader(
      version,
      parentId,
      adProofsRoot,
      candidateBlock.stateRoot,
      txsRoot,
      candidateBlock.timestamp,
      height,
      0L,
      candidateBlock.difficulty,
      EquihashSolution.empty
    )

    @tailrec
    def generateHeader(nonce: Long): Option[EncryBlockHeader] = {
      val currentDigest = new Blake2bDigest(digest)
      Equihash.hashNonce(currentDigest, nonce)
      val solutions = Equihash.gbpBasic(currentDigest, n, k)
      val headerWithSuitableSolution = solutions
        .map { solution => h.copy(nonce = nonce, equihashSolution = solution) }
        .find { newHeader => correctWorkDone(realDifficulty(newHeader), difficulty) }
      headerWithSuitableSolution match {
        case headerWithFoundSolution @ Some(_) => headerWithFoundSolution
        case None if nonce + 1 < finishingNonce => generateHeader(nonce + 1)
        case _ => None
      }
    }

    val possibleHeader = generateHeader(startingNonce)

    possibleHeader.flatMap(header => {
      if (verify(header)) {
        val adProofs = ADProofs(header.id, candidateBlock.adProofBytes)
        val payload = EncryBlockPayload(header.id, candidateBlock.transactions)
        Some(EncryBlock(header, payload, Some(adProofs)))
      } else None
    })
  }

  def verify(header: EncryBlockHeader): Boolean =
    Equihash.validateSolution(
      n,
      k,
      seed,
      Equihash.nonceToLeBytes(header.nonce),
      header.equihashSolution.indexedSeq
    )

  override def getDerivedHeaderFields(parentOpt: Option[EncryBlockHeader],
                                      adProofBytes: SerializedAdProof,
                                      transactions: Seq[Transaction]): (Byte, ModifierId, Digest32, Digest32, Int) = {
    val version: Version = Constants.Chain.Version
    val parentId: ModifierId = parentOpt.map(_.id).getOrElse(EncryBlockHeader.GenesisParentId)
    val adProofsRoot: Digest32 = ADProofs.proofDigest(adProofBytes)
    val txsRoot: Digest32 = EncryBlockPayload.rootHash(transactions.map(_.id))
    val height: Int = parentOpt.map(_.height).getOrElse(Constants.Chain.PreGenesisHeight) + 1

    (version, parentId, adProofsRoot, txsRoot, height)
  }

  override def realDifficulty(header: EncryBlockHeader): Difficulty = {
    Difficulty @@ (Constants.Chain.MaxTarget / BigInt(1, header.powHash))
  }

  override def toString: String = s"EquihashPowScheme(n = ${n.toInt}, k = ${k.toInt})"
}
