package encry.local.explorer

import akka.actor.Actor
import encry.ModifierId
import encry.local.explorer.BlockListener.{ChainSwitching, NewOrphaned}
import encry.local.explorer.database.DBService
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.EncryApp.settings
import encry.stats.LoggingActor.LogMessage

class BlockListener(dBService: DBService) extends Actor {

  override def preStart(): Unit = {
    if(settings.logging.enableLogging)
      context.system.actorSelection("/user/loggingActor") ! LogMessage("Info", "Start listening to new blocks.", System.currentTimeMillis())
    context.system.eventStream.subscribe(context.self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(block: EncryBlock) => dBService.processBlock(block)
    case NewOrphaned(header: EncryBlockHeader) => dBService.processOrphanedHeader(header)
    case ChainSwitching(ids) => dBService.markAsRemovedFromMainChain(ids.toList)
  }
}

object BlockListener {
  case class ChainSwitching(switchedIds: Seq[ModifierId])
  case class NewOrphaned(header: EncryBlockHeader)
}
