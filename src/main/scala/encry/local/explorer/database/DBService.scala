package encry.local.explorer.database

import cats.effect.IO
import doobie.free.connection.ConnectionIO
import doobie.implicits._
import encry.EncryApp.settings
import encry.ModifierId
import QueryRepository._
import com.zaxxer.hikari.HikariDataSource
import doobie.hikari.HikariTransactor
import encry.modifiers.history.block.EncryBlock
import encry.utils.Logging
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

trait DBService {
  def processBlock(block: EncryBlock): Future[Int]
  def processHeader(block: EncryBlock): Future[Int]
  def markAsRemovedFromMainChain(ids: List[ModifierId]): Future[Int]
}

class DBServiceImpl extends DBService with Logging {

  def processBlock(block: EncryBlock): Future[Int] = runAsync(processBlockQuery(block))

  def processHeader(block: EncryBlock): Future[Int] = runAsync(insertHeaderQuery(block))

  def markAsRemovedFromMainChain(ids: List[ModifierId]): Future[Int] = runAsync(markAsRemovedFromMainChainQuery(ids))

  private lazy val dataSource = new HikariDataSource
  if (settings.postgres.enabled) {
    dataSource.setJdbcUrl(settings.postgres.host)
    dataSource.setUsername(settings.postgres.user)
    dataSource.setPassword(settings.postgres.password)
    dataSource.setMaximumPoolSize(5)
  }

  private lazy val pgTransactor: HikariTransactor[IO] = HikariTransactor[IO](dataSource)

  private def runAsync(io: ConnectionIO[Int]): Future[Int] =
    if (settings.postgres.enabled) {
      (for {
        res <- io.transact(pgTransactor)
      } yield res)
        .unsafeToFuture()
        .recoverWith {
          case NonFatal(th) =>
            log.warn("Failed to perform db operation", th)
            Future.failed(th)
        }
    } else Future.successful(0)
}
