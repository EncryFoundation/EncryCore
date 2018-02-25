package encry.consensus

import encry.crypto.{PublicKey25519, Signature25519}
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.EncryBaseTransaction
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.core.block.Block.Timestamp
import scorex.core.serialization.JsonSerializable
import scorex.crypto.authds.{ADDigest, SerializedAdProof}

class PowCandidateBlock(val accountPubKey: PublicKey25519,
                        val signature: Signature25519,
                        val parentOpt: Option[EncryBlockHeader],
                        val adProofBytes: SerializedAdProof,
                        val stateRoot: ADDigest,
                        val transactions: Seq[EncryBaseTransaction],
                        val timestamp: Timestamp,
                        val difficulty: Difficulty) extends JsonSerializable {

  override lazy val json: Json = Map(
    "minerProposition" -> Algos.encode(accountPubKey.pubKeyBytes).asJson,
    "signature" -> Algos.encode(signature.signature).asJson,
    "parentId" -> parentOpt.map(p => Algos.encode(p.id)).getOrElse("None").asJson,
    "stateRoot" -> Algos.encode(stateRoot).asJson,
    "adProofBytes" -> Algos.encode(adProofBytes).asJson,
    "timestamp" -> timestamp.asJson,
    "transactions" -> transactions.map(_.json).asJson,
    "transactionsQty" -> transactions.length.asJson,
  ).asJson

  override def toString: String = s"<CandidateBlock timestamp=$timestamp txQty=${transactions.size} " +
    s"parentId=${parentOpt.map(_.encodedId).getOrElse("None")}>"
}
