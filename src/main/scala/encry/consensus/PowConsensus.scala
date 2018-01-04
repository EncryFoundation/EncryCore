package encry.consensus

import encry.consensus.validation.PowConsensusValidator
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.settings.ChainSettings
import scorex.core.ModifierId

class PowConsensus(chainSettings: ChainSettings) {

  val difficultyController: PowLinearController = new PowLinearController(chainSettings)
  val validator: PowConsensusValidator.type = PowConsensusValidator

  def verifyCandidate(candidate: PowCandidateBlock, nonce: Long): Option[EncryBlock] = {

    // TODO: Move to settings.
    val version = 0.toByte
    val parentId: ModifierId = ModifierId @@ candidate.parentOpt.map(_.id).getOrElse(EncryBlockHeader.GenesisParentId)
    val adProofsRoot = ADProofs.proofDigest(candidate.adProofBytes)
    val txRoot = EncryBlockPayload.rootHash(candidate.transactions.map(_.id))
    val height = candidate.parentOpt.map(_.height + 1).getOrElse(0)

    val header = EncryBlockHeader(
      version,
      parentId,
      adProofsRoot,
      candidate.stateRoot,
      txRoot,
      candidate.timestamp,
      height,
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
