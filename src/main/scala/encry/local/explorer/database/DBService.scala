package encry.local.explorer.database

import cats.effect.IO
import doobie.free.connection.ConnectionIO
import doobie.implicits._
import doobie.util.transactor.Transactor
import doobie.util.transactor.Transactor.Aux
import encry.EncryApp.settings
import encry.ModifierId
import QueryRepository._
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader

import scala.concurrent.Future

trait DBService {
  def processBlock(block: EncryBlock): Future[Int]
  def processHeader(header: EncryBlockHeader): Future[Int]
  def markAsRemovedFromMainChain(ids: List[ModifierId]): Future[Int]
}

class DBServiceImpl extends DBService {

  def processBlock(block: EncryBlock): Future[Int] = runAsync(processBlockQuery(block))

  def processHeader(header: EncryBlockHeader): Future[Int] = runAsync(insertHeaderQuery(header))

  def markAsRemovedFromMainChain(ids: List[ModifierId]): Future[Int] = runAsync(markAsRemovedFromMainChainQuery(ids))

  // internal

  private val transactor: Aux[IO, Unit] = Transactor
    .fromDriverManager[IO](driver = "org.postgresql.Driver",
                           url = settings.postgres.host,
                           user = settings.postgres.user,
                           pass = settings.postgres.password)

  private def runAsync[T](io: ConnectionIO[T]): Future[T] = io.transact(transactor).unsafeToFuture
}
