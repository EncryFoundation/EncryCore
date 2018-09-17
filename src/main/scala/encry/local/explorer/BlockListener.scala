package encry.local.explorer

import akka.actor.Actor
import encry.utils.CoreTaggedTypes.ModifierId
import encry.local.explorer.BlockListener.{ChainSwitching, NewOrphaned}
import encry.local.explorer.database.DBService
import encry.modifiers.history.{Block, Header}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.utils.Logging

class BlockListener(dBService: DBService) extends Actor with Logging {

  override def preStart(): Unit = {
    logInfo(s"Start listening to new blocks.")
    context.system.eventStream.subscribe(context.self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(block: Block) => dBService.processBlock(block)
    case NewOrphaned(header: Header) => dBService.processOrphanedHeader(header)
    case ChainSwitching(ids) => dBService.markAsRemovedFromMainChain(ids.toList)
  }
}

object BlockListener {
  case class ChainSwitching(switchedIds: Seq[ModifierId])
  case class NewOrphaned(header: Header)
}
