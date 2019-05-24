package encry.consensus

import io.circe.Encoder
import io.circe.syntax._
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, Difficulty, SerializedAdProof}

case class CandidateBlock(parentOpt: Option[Header],
                          adProofBytes: SerializedAdProof,
                          stateRoot: ADDigest,
                          version: Byte,
                          transactions: Seq[Transaction],
                          timestamp: Long,
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