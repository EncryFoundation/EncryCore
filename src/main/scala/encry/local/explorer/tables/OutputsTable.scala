package encry.local.explorer.tables

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.{AssetBox, DataBox}
import encry.settings.Constants
import scorex.crypto.encode.Base16

object OutputsTable {

  val name: String = "outputs"
  val fields: Seq[String] = Seq("id", "tx_id", "monetary_value", "coin_id", "contract_hash", "data")
  val fieldsString: String = fields.mkString("(", ", ", ")")

  def dataString(h: EncryBlockHeader, tx: EncryBaseTransaction): String = {
    val txId: String = Base16.encode(tx.id)
    val outputs: IndexedSeq[String] = tx.newBoxes.map { bx =>
      val id: String = Base16.encode(bx.id)
      val (monetaryValue: Long, coinId: String, dataOpt: Option[Array[Byte]]) = bx match {
        case ab: AssetBox => (ab.amount, Base16.encode(ab.tokenIdOpt.getOrElse(Constants.IntrinsicTokenId)), None)
        case db: DataBox => (0L, Base16.encode(Constants.IntrinsicTokenId), db.data)
        case _ => (0L, Base16.encode(Constants.IntrinsicTokenId), None)
      }
      val data: String = dataOpt.fold("{}")(_.mkString("{", ", ", "}"))
      val contractHash: String = Base16.encode(bx.proposition.contractHash)
      s"('$id', '$txId', '$monetaryValue', '$coinId', '$contractHash', '$data')"
    }.toIndexedSeq
    outputs.mkString(", ")
  }

  def dataStrings(h: EncryBlockHeader, p: EncryBlockPayload): String =
    p.transactions.map(tx => dataString(h, tx)).mkString(", ")
}
