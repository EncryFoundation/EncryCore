package encry.nvg

import akka.actor.{ Actor, ActorRef }
import akka.routing.BalancingPool
import com.typesafe.scalalogging.StrictLogging
import encry.network.DownloadedModifiersValidator.ModifiersForValidating
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.settings.EncryAppSettings

class IntermediaryNVH(settings: EncryAppSettings) extends Actor with StrictLogging {

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

  override def receive: Receive = {
    case msg @ ModifiersForValidating(_, _, _) => modifiersValidatorRouter ! msg
    case msg @ DataFromPeer(_, _)              => networkMessagesProcessor ! msg
    case _                                     =>
  }
}

object IntermediaryNVH {}
