package encry.consensus

import encry.consensus.validation.PowConsensusValidator
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.settings.ChainSettings
import scorex.core.ModifierId
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.hash.Digest32

class PowConsensus(chainSettings: ChainSettings) {

  import PowConsensus._

  val difficultyController: PowLinearController = new PowLinearController(chainSettings)
  val validator: PowConsensusValidator.type = PowConsensusValidator

  def verifyCandidate(candidate: PowCandidateBlock, nonce: Long): Option[EncryBlock] = {

    val derivedHeaderFields =
      getDerivedHeaderFields(candidate.parentOpt, candidate.adProofBytes, candidate.transactions)

    val header = EncryBlockHeader(
      derivedHeaderFields._1,
      candidate.proposition,
      candidate.signature,
      derivedHeaderFields._2,
      derivedHeaderFields._3,
      candidate.stateRoot,
      derivedHeaderFields._4,
      candidate.timestamp,
      derivedHeaderFields._5,
      nonce,
      candidate.difficulty,
    )

    if (validator.validatePow(header.hHash, candidate.difficulty)) {
      val adProofs = ADProofs(header.id, candidate.adProofBytes)
      val payload = new EncryBlockPayload(header.id, candidate.transactions)
      Option(new EncryBlock(header, payload, Some(adProofs)))
    } else None
  }
}

object PowConsensus {

  def getDerivedHeaderFields(parentOpt: Option[EncryBlockHeader], adProofBytes: SerializedAdProof,
                             transactions: Seq[EncryBaseTransaction]): (Byte, ModifierId, Digest32, Digest32, Int) = {
    // TODO: Move to settings.
    val version = 1.toByte
    val parentId: ModifierId = ModifierId @@ parentOpt.map(_.id).getOrElse(EncryBlockHeader.GenesisParentId)
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)
    val txsRoot = EncryBlockPayload.rootHash(transactions.map(_.id))
    val height = parentOpt.map(_.height).getOrElse(0) + 1

    (version, parentId, adProofsRoot, txsRoot, height)
  }
}
