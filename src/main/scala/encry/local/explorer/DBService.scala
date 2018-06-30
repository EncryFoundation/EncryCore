package encry.local.explorer

import cats.effect.IO
import cats.implicits._
import doobie.free.connection.ConnectionIO
import doobie.implicits._
import doobie.util.fragment.Fragment
import doobie.util.transactor.Transactor
import doobie.util.update.Update
import encry.ModifierId
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import scorex.crypto.encode.Base16

object DBService {

  def insert(table: String, fieldsString: String, dataString: String): ConnectionIO[Int] =
    Fragment.const(s"INSERT INTO $table $fieldsString VALUES $dataString;").update.run

  def insertBlock(block: EncryBlock): ConnectionIO[Int] =
    insert(tables.HeadersTable.name, tables.HeadersTable.fieldsString, tables.HeadersTable.dataString(block))

  def insertTransactions(h: EncryBlockHeader, p: EncryBlockPayload): ConnectionIO[Int] =
    insert(tables.TransactionsTable.name, tables.TransactionsTable.fieldsString, tables.TransactionsTable.dataStrings(h, p))

  def insertInputs(h: EncryBlockHeader, p: EncryBlockPayload): ConnectionIO[Int] =
    insert(tables.InputsTable.name, tables.InputsTable.fieldsString, tables.InputsTable.dataStrings(h, p))

  def insertOutputs(h: EncryBlockHeader, p: EncryBlockPayload): ConnectionIO[Int] =
    insert(tables.OutputsTable.name, tables.OutputsTable.fieldsString, tables.OutputsTable.dataStrings(h, p))

  def processBlock(block: EncryBlock, transactor: Transactor[IO]): IO[Int] = (for {
    blockR <- insertBlock(block)
    txsR   <- insertTransactions(block.header, block.payload)
    outsR  <- insertOutputs(block.header, block.payload)
    insR   <- insertInputs(block.header, block.payload)
  } yield txsR + blockR + outsR + insR).transact(transactor)

  def markAsRemovedFromMainChain(ids: List[ModifierId], transactor: Transactor[IO]): IO[Int] =
    Update[String](tables.HeadersTable.updateByIdSql("best_chain = FALSE"))
      .updateMany(ids.map(Base16.encode))
      .transact(transactor)
}
