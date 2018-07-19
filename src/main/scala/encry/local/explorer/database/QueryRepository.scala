package encry.local.explorer.database

import doobie.free.connection.ConnectionIO
import doobie.util.fragment.Fragment
import doobie.util.update.Update
import encry.ModifierId
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import scorex.crypto.encode.Base16
import cats.implicits._

protected[database] object QueryRepository {

  def processBlockQuery(block: EncryBlock): ConnectionIO[Int] =
    for {
      blockR <- insertBlockQuery(block)
      txsR   <- insertTransactionsQuery(block.header, block.payload)
      outsR  <- insertOutputsQuery(block.header, block.payload)
      insR   <- insertInputsQuery(block.header, block.payload)
    } yield txsR + blockR + outsR + insR

  def insertHeaderQuery(header: EncryBlockHeader): ConnectionIO[Int] =
    insertQuery(tables.HeadersTable.name, tables.HeadersTable.fieldsString, tables.HeadersTable.dataString(header))

  def markAsRemovedFromMainChainQuery(ids: List[ModifierId]): ConnectionIO[Int] =
    Update[String](tables.HeadersTable.updateByIdSql("best_chain = FALSE"))
      .updateMany(ids.map(Base16.encode))

  // internal

  private def insertQuery(table: String, fieldsString: String, dataString: String): ConnectionIO[Int] =
    Fragment.const(s"INSERT INTO $table $fieldsString VALUES $dataString;").update.run

  private def insertBlockQuery(block: EncryBlock): ConnectionIO[Int] =
    insertQuery(tables.HeadersTable.name, tables.HeadersTable.fieldsString, tables.HeadersTable.dataString(block))

  private def insertTransactionsQuery(h: EncryBlockHeader, p: EncryBlockPayload): ConnectionIO[Int] =
    insertQuery(tables.TransactionsTable.name, tables.TransactionsTable.fieldsString, tables.TransactionsTable.dataStrings(h, p))

  private def insertInputsQuery(h: EncryBlockHeader, p: EncryBlockPayload): ConnectionIO[Int] =
    insertQuery(tables.InputsTable.name, tables.InputsTable.fieldsString, tables.InputsTable.dataStrings(h, p))

  private def insertOutputsQuery(h: EncryBlockHeader, p: EncryBlockPayload): ConnectionIO[Int] =
    insertQuery(tables.OutputsTable.name, tables.OutputsTable.fieldsString, tables.OutputsTable.dataStrings(h, p))

}
