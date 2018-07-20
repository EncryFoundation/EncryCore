package encry.local.explorer

import akka.actor.Actor
import encry.ModifierId
import encry.local.explorer.BlockListener.{ChainSwitching, NewOrphaned}
import encry.local.explorer.database.DBService
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.utils.Logging

class BlockListener(dBService: DBService) extends Actor with Logging {

  override def preStart(): Unit = {
    logger.info("Start listening to new blocks.")
    context.system.eventStream.subscribe(context.self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(block: EncryBlock) => dBService.processBlock(block)
    case NewOrphaned(header: EncryBlockHeader) => dBService.processHeader(header)
    case ChainSwitching(ids) => dBService.markAsRemovedFromMainChain(ids.toList)
  }
}

object BlockListener {
  case class ChainSwitching(switchedIds: Seq[ModifierId])
  case class NewOrphaned(header: EncryBlockHeader)

  val name: String = "blockListener"
}
