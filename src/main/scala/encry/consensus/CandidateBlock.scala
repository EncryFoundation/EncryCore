package encry.consensus

import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.modifiers.history.Block.{Timestamp, Version}
import encry.modifiers.history.Header
import encry.modifiers.mempool.Transaction
import io.circe.Encoder
import io.circe.syntax._
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, SerializedAdProof}

case class CandidateBlock(parentOpt: Option[Header],
                          adProofBytes: SerializedAdProof,
                          stateRoot: ADDigest,
                          version: Version,
                          transactions: Seq[Transaction],
                          timestamp: Timestamp,
                          difficulty: Difficulty) {

  override def toString: String = s"<CandidateBlock timestamp=$timestamp txQty=${transactions.size} " +
    s"parentId=${parentOpt.map(_.encodedId).getOrElse("None")}>"
}

object CandidateBlock {

  implicit val jsonEncoder: Encoder[CandidateBlock] = (b: CandidateBlock) => Map(
    "parentId" -> b.parentOpt.map(p => Algos.encode(p.id)).getOrElse("None").asJson,
    "stateRoot" -> Algos.encode(b.stateRoot).asJson,
    "adProofBytes" -> Algos.encode(b.adProofBytes).asJson,
    "timestamp" -> b.timestamp.asJson,
    "transactions" -> b.transactions.map(_.asJson).asJson,
    "transactionsQty" -> b.transactions.length.asJson,
  ).asJson
}
