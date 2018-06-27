package encry.local.explorer

import cats.effect.IO
import doobie.implicits._
import doobie.free.connection.ConnectionIO
import doobie.util.fragment.Fragment
import doobie.util.transactor.Transactor
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload

object DBService {

  def insert(table: String, fieldsString: String, dataString: String): ConnectionIO[Int] =
    Fragment.const(s"INSERT INTO $table $fieldsString VALUES $dataString;").update.run

  def insertBlock(block: EncryBlock): ConnectionIO[Int] = {
    import tables.BlocksTable._

    insert(name, fieldsString, dataString(block))
  }

  def insertTransactions(h: EncryBlockHeader, p: EncryBlockPayload): ConnectionIO[Int] = {
    import tables.TransactionsTable._

    insert(name, fieldsString, dataStrings(h, p))
  }

  def insertInputs(h: EncryBlockHeader, p: EncryBlockPayload): ConnectionIO[Int] = {
    import tables.InputsTable._

    insert(name, fieldsString, dataStrings(h, p))
  }

  def insertOutputs(h: EncryBlockHeader, p: EncryBlockPayload): ConnectionIO[Int] = {
    import tables.OutputsTable._

    insert(name, fieldsString, dataStrings(h, p))
  }

  def processBlock(block: EncryBlock, transactor: Transactor[IO]): IO[Int] = (for {
    blockR <- insertBlock(block)
    txsR <- insertTransactions(block.header, block.payload)
    outsR <- insertOutputs(block.header, block.payload)
    insR <- insertInputs(block.header, block.payload)
  } yield txsR + blockR + outsR + insR).transact(transactor)
}
