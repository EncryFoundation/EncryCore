package encry.consensus

import com.google.common.primitives.Chars
import encry.crypto.PublicKey25519
import encry.crypto.equihash.{Equihash, EquihashSolution}
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.state.box.proof.Signature25519
import encry.settings.Constants
import org.bouncycastle.crypto.digests.Blake2bDigest
import scorex.core.ModifierId
import scorex.core.block.Block.{Timestamp, Version}
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.annotation.tailrec
import scala.math.BigInt

class EquihashPowScheme(n: Char, k: Char) extends ConsensusScheme with ScorexLogging {
  lazy val ergoPerson: Array[Byte] = "ERGOPoWT1234".getBytes("UTF-8") ++
    Chars.toByteArray(n) ++
    Chars.toByteArray(k)

  @SuppressWarnings(Array("NullParameter"))
  override def prove(parentOpt: Option[EncryBlockHeader],
                     nBits: NBits,
                     stateRoot: ADDigest,
                     adProofsRoot: Digest32,
                     transactionsRoot: Digest32,
                     timestamp: Timestamp,
                     version: Version,
                     accountPubKey: PublicKey25519,
                     signature: Signature25519,
                     startingNonce: Long,
                     finishingNonce: Long,
                    ): Option[EncryBlockHeader] = {
    require(finishingNonce >= startingNonce)

    val difficulty = DifficultySerializer.decodeCompactBits(nBits)

    val (parentId, version, height) = derivedHeaderFields(parentOpt)

    val bytesPerWord = n / 8
    val wordsPerHash = 512 / n

    val digest = new Blake2bDigest(null, bytesPerWord * wordsPerHash, null, ergoPerson) // scalastyle:ignore
    val h = EncryBlockHeader(version, accountPubKey, signature, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      height, 0L, nBits, EquihashSolution.empty)

    @tailrec
    def generateHeader(nonce: Long): Option[EncryBlockHeader] = {
      log.debug("Trying nonce: " + nonce)
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

    generateHeader(startingNonce)
  }

  override def verify(header: EncryBlockHeader): Boolean =
    Equihash.validateSolution(
      n,
      k,
      ergoPerson,
      Equihash.nonceToLeBytes(header.nonce),
      header.equihashSolution.indexedSeq
    )

  override def derivedHeaderFields(parentOpt: Option[EncryBlockHeader]): (ModifierId, Byte, Int) = {

    val height = parentOpt.map(parent => parent.height + 1).getOrElse(0)

    val version = 0: Byte

    val parentId: ModifierId = parentOpt.map(_.id).getOrElse(EncryBlockHeader.GenesisParentId)

    (parentId, version, height)
  }


  override def realDifficulty(header: EncryBlockHeader): Difficulty = Difficulty @@ (Constants.Chain.MaxTarget / BigInt(1, header.powHash))

  override def toString: String = s"EquihashPowScheme(n = ${n.toInt}, k = ${k.toInt})"
}
