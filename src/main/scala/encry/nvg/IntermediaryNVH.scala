package encry.nvg

import akka.actor.{ Actor, ActorRef }
import akka.routing.BalancingPool
import com.typesafe.scalalogging.StrictLogging
import encry.network.BlackList.BanReason.CorruptedSerializedBytes
import encry.network.DownloadedModifiersValidator.{ InvalidModifier, ModifiersForValidating }
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.PeersKeeper.BanPeer
import encry.nvg.ModifiersValidator.ModifierForValidation
import encry.nvg.NodeViewHolder.UpdateHistoryReader
import encry.settings.EncryAppSettings
import encry.view.history.HistoryReader
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

class IntermediaryNVH(settings: EncryAppSettings, intermediaryNetwork: ActorRef) extends Actor with StrictLogging {

  val networkMessagesProcessor: ActorRef =
    context.actorOf(NetworkMessagesProcessor.props, name = "Network-messages-processor")
  val nodeViewHolder: ActorRef =
    context.actorOf(NodeViewHolder.props, name = "Node-view-holder")
  val modifiersValidatorRouter: ActorRef =
    context.actorOf(
      BalancingPool(5)
        .props(ModifiersValidator.props(nodeViewHolder, settings)),
      name = "Modifiers-validator-router"
    )

  var historyReader: HistoryReader = HistoryReader.empty

  override def receive: Receive = {
    case ModifiersForValidating(remote, typeId, modifiers) =>
      logger.info(s"Got ${modifiers.size} modifiers of type $typeId for validation.")
      modifiers.foreach {
        case (id: ModifierId, bytes: Array[Byte]) =>
          modifiersValidatorRouter ! ModifierForValidation(historyReader, id, typeId, bytes, remote)
      }
    case msg @ DataFromPeer(_, _) => networkMessagesProcessor ! msg
    case UpdateHistoryReader(newReader: HistoryReader) =>
      historyReader = newReader
      networkMessagesProcessor ! newReader
    case msg @ BanPeer(_, _)      => networkMessagesProcessor ! msg
    case msg @ InvalidModifier(_) => networkMessagesProcessor ! msg
  }
}

object IntermediaryNVH {}
