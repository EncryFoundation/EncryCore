package encry.local.explorer.database

import doobie.free.connection.ConnectionIO
import doobie.util.update.Update
import encry.ModifierId
import doobie.implicits._
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import scorex.crypto.encode.Base16
import cats.implicits._
import encry.local.explorer.database.Tables.{HeadersTable, InputsTable, OutputsTable, TransactionsTable}

protected[database] object QueryRepository {

  def processBlockQuery(block: EncryBlock): ConnectionIO[Int] =
    for {
      blockR <- insertBlockQuery(block)
      txsR   <- insertTransactionsQuery(block.header, block.payload)
      outsR  <- insertOutputsQuery(block.payload)
      insR   <- insertInputsQuery(block.payload)
    } yield txsR + blockR + outsR + insR

  def insertHeaderQuery(header: EncryBlockHeader): ConnectionIO[Int] =
    insertQuery(HeadersTable.name, HeadersTable.allFields, HeadersTable.dataString(header))

  def markAsRemovedFromMainChainQuery(ids: List[ModifierId]): ConnectionIO[Int] = {
    val query = "UPDATE ${HeadersTable.name} SET best_chain = FALSE WHERE id = ?"
    Update[String](query).updateMany(ids.map(Base16.encode))
  }

  // internal

  private def insertQuery(table: String, fieldsString: String, dataString: String): ConnectionIO[Int] =
    sql"INSERT INTO $table $fieldsString VALUES $dataString;".update.run

  private def insertBlockQuery(block: EncryBlock): ConnectionIO[Int] =
    insertQuery(HeadersTable.name, HeadersTable.allFields, HeadersTable.dataString(block))

  private def insertTransactionsQuery(h: EncryBlockHeader, p: EncryBlockPayload): ConnectionIO[Int] =
    insertQuery(TransactionsTable.name, TransactionsTable.allFields, TransactionsTable.dataStrings(h, p))

  private def insertInputsQuery(p: EncryBlockPayload): ConnectionIO[Int] =
    insertQuery(InputsTable.name, InputsTable.allFields, InputsTable.dataString(p))

  private def insertOutputsQuery(p: EncryBlockPayload): ConnectionIO[Int] =
    insertQuery(OutputsTable.name, OutputsTable.allFields, OutputsTable.dataString(p))

}
