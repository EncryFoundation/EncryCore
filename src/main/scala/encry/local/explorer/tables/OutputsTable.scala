package encry.local.explorer.tables

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.AssetBox
import encry.settings.{Algos, Constants}

object OutputsTable {

  val name: String = "outputs"
  val fields: Seq[String] = Seq("id", "tx_id", "value", "coin_id", "contract_hash")
  val fieldsString: String = fields.mkString("(", ", ", ")")

  def dataString(h: EncryBlockHeader, tx: EncryBaseTransaction): String = {
    val txId: String = Algos.encode(tx.id)
    val outputs: IndexedSeq[String] = tx.newBoxes.map { bx =>
      val id: String = Algos.encode(bx.id)
      val (monetaryVal: Long, coinId: String) = bx match {
        case ab: AssetBox => ab.amount -> Algos.encode(ab.tokenIdOpt.getOrElse(Constants.IntrinsicTokenId))
        case _ => 0L -> Algos.encode(Constants.IntrinsicTokenId)
      }
      val contractHash: String = Algos.encode(bx.proposition.contractHash)
      s"('$id', '$txId', '$monetaryVal', '$coinId', '$contractHash')"
    }.toIndexedSeq
    outputs.mkString(", ")
  }

  def dataStrings(h: EncryBlockHeader, p: EncryBlockPayload): String =
    p.transactions.map(tx => dataString(h, tx)).mkString(", ")
}
