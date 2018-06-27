package encry.local.explorer.tables

import com.google.common.primitives.Ints
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.settings.Algos

object InputsTable {

  val name: String = "outputs"
  val fields: Seq[String] = Seq("id", "tx_id", "serialized_proofs")
  val fieldsString: String = fields.mkString("(", ", ", ")")

  def dataString(h: EncryBlockHeader, tx: EncryBaseTransaction): String = {
    val txId: String = Algos.encode(tx.id)
    val inputs: IndexedSeq[String] = tx.inputs.map { in =>
      val id: String = Algos.encode(in.boxId)
      val proofs: String = Algos.encode(in.proofs.map { proof =>
        val proofBytes: Array[Byte] = proof.bytes
        Ints.toByteArray(proofBytes.length) ++ proofBytes
      }.foldLeft(Array.empty[Byte])(_ ++ _))
      s"('$id', '$txId', '$proofs')"
    }
    inputs.mkString(", ")
  }

  def dataStrings(h: EncryBlockHeader, p: EncryBlockPayload): String =
    p.transactions.map(tx => dataString(h, tx)).mkString(", ")
}
