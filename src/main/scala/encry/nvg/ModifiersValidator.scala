package encry.nvg

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.network.DownloadedModifiersValidator.ModifiersForValidating
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.settings.EncryAppSettings
import org.encryfoundation.common.network.BasicMessagesRepo.ModifiersNetworkMessage

class ModifiersValidator(nodeViewHolderRef: ActorRef, settings: EncryAppSettings) extends Actor with StrictLogging {
  override def receive: Receive = {
    case ModifiersForValidating(remote, typeId, filteredModifiers) =>
    case _ =>
  }
}

object ModifiersValidator {
  def props(nodeViewHolderRef: ActorRef, settings: EncryAppSettings): Props =
    Props(new ModifiersValidator(nodeViewHolderRef, settings))
}
