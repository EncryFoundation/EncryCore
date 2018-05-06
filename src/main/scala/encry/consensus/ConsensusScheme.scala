package encry.consensus

import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.settings.Algos
import scorex.core.ModifierId
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.{ADDigest, LeafData, SerializedAdProof}
import scorex.crypto.hash.Digest32

import scala.math.BigInt

trait ConsensusScheme {

  def prove(parentOpt: Option[ModifierId],
            nBits: Long,
            stateRoot: ADDigest,
            adProofsRoot: Digest32,
            transactionsRoot: Digest32,
            timestamp: Timestamp,
            startingNonce: Long,
            finishingNonce: Long
           ): Option[EncryBlockHeader]

  def proveBlock(parentOpt: Option[ModifierId],
                 nBits: Long,
                 stateRoot: ADDigest,
                 adProofBytes: SerializedAdProof,
                 transactions: Seq[EncryBaseTransaction],
                 timestamp: Timestamp,
                 startingNonce: Long,
                 finishingNonce: Long): Option[EncryBlock] = {

    val transactionsRoot = Algos.merkleTreeRoot(LeafData @@ transactions.map(_.id.untag(ModifierId)))
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)

    prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot,
      timestamp, startingNonce, finishingNonce).map { h =>
      val adProofs = ADProofs(h.id, adProofBytes)
      new EncryBlock(h, EncryBlockPayload(h.id, transactions), Some(adProofs))
    }
  }

  def proveBlock(candidateBlock: EncryBlock,
                 nonce: Long): Option[EncryBlock] = {

    val parentOpt: Option[ModifierId] = Some(candidateBlock.header.parentId)
    val nBits: Long = candidateBlock.header.nBits
    val stateRoot: ADDigest = candidateBlock.header.stateRoot
    val adProofBytes: SerializedAdProof = SerializedAdProof @@ candidateBlock.adProofsOpt.map(_.bytes).getOrElse(Array.emptyByteArray)
    val transactions: Seq[EncryBaseTransaction] = candidateBlock.transactions
    val timestamp: Timestamp = candidateBlock.header.timestamp

    val transactionsRoot = Algos.merkleTreeRoot(LeafData @@ transactions.map(_.id.untag(ModifierId)))
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)

    prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot, timestamp, nonce, nonce).map { h =>
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