package encry.consensus

import encry.crypto.PublicKey25519
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proof.Signature25519
import encry.settings.Algos
import scorex.core.ModifierId
import scorex.core.block.Block.{Timestamp, Version}
import scorex.crypto.authds.{ADDigest, LeafData, SerializedAdProof}
import scorex.crypto.hash.Digest32

import scala.math.BigInt

trait ConsensusScheme {

  def prove(parentOpt: Option[EncryBlockHeader],
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
           ): Option[EncryBlockHeader]

  def proveBlock(parentOpt: Option[EncryBlockHeader],
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
                 transactions: Seq[EncryBaseTransaction],
                 aDProofs: ADProofs): Option[EncryBlock] = {

    //TODO: Replace parantId with correct
    prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot, timestamp, version, accountPubKey, signature, startingNonce, finishingNonce).map { h =>
      new EncryBlock(h, EncryBlockPayload(h.id, transactions), Some(aDProofs))
    }
  }

  def proveBlock(candidateBlock: PowCandidateBlock,
                 nonce: Long): Option[EncryBlock] = {

    val parentOpt: Option[EncryBlockHeader] = candidateBlock.parentOpt
    val nBits: NBits = candidateBlock.nBits
    val stateRoot: ADDigest = candidateBlock.stateRoot
    val adProofBytes: SerializedAdProof = candidateBlock.adProofBytes
    val transactions: Seq[EncryBaseTransaction] = candidateBlock.transactions
    val timestamp: Timestamp = candidateBlock.timestamp
    val version: Version = candidateBlock.version
    val accountPubKey: PublicKey25519 = candidateBlock.accountPubKey
    val signature: Signature25519 = candidateBlock.signature

    val transactionsRoot = Algos.merkleTreeRoot(LeafData @@ transactions.map(_.id.untag(ModifierId)))
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)

    prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot, timestamp, version, accountPubKey, signature, nonce, nonce).map { h =>
      val adProofs = ADProofs(h.id, adProofBytes)
      new EncryBlock(h, EncryBlockPayload(h.id, transactions), Some(adProofs))
    }
  }

  def verify(header: EncryBlockHeader): Boolean

  def realDifficulty(header: EncryBlockHeader): BigInt

  protected def derivedHeaderFields(parentOpt: Option[EncryBlockHeader]): (ModifierId, Byte, Int) = {

    val height = parentOpt.map(parent => parent.height + 1).getOrElse(0)

    val version = 0: Byte

    val parentId: ModifierId = parentOpt.map(_.id).getOrElse(EncryBlockHeader.GenesisParentId)

    (parentId, version, height)
  }

  def correctWorkDone(realDifficulty: Difficulty, difficulty: BigInt): Boolean = {
    realDifficulty >= difficulty
  }
}