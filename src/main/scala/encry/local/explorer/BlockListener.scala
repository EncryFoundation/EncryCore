package encry.local.explorer

import akka.actor.Actor
import cats.effect.IO
import doobie.hikari.HikariTransactor
import encry.EncryApp.settings
import encry.ModifierId
import encry.local.explorer.BlockListener.ChainSwitching
import encry.modifiers.history.block.EncryBlock
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.utils.ScorexLogging

class BlockListener extends Actor with ScorexLogging {

  val transactor: HikariTransactor[IO] = HikariTransactor
    .newHikariTransactor[IO](
      driverClassName = "org.postgresql.Driver",
      url = settings.postgres.host,
      user = settings.postgres.user,
      pass = settings.postgres.password
    ).map { ht => ht.configure(_ => IO(())); ht }
    .unsafeRunSync()

  override def preStart(): Unit = {
    logger.info("Start listening to new blocks.")
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(block: EncryBlock) => DBService.processBlock(block, transactor).unsafeRunSync()
    case ChainSwitching(ids) => DBService.markAsRemovedFromMainChain(ids.toList, transactor).unsafeRunSync()
  }
}

object BlockListener {
  case class ChainSwitching(switchedIds: Seq[ModifierId])
}
