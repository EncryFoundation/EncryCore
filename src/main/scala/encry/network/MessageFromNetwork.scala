package encry.network

import encry.network.PeerConnectionHandler.ConnectedPeer
import org.encryfoundation.common.network.BasicMessagesRepo.NetworkMessage

/**
  * @param message - message, received from network
  * @param source - sender of received message
  *
  *               This case class transfers network message from PeerConnectionHandler actor to the NetworkController.
  *               Main duty is to transfer message from network with sender of it message to the NetworkController as an end point.
  */

case class MessageFromNetwork(message: NetworkMessage, source: Option[ConnectedPeer])