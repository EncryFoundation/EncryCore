package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import encry.consensus.History.Older
import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header}
import encry.network.BasicMessagesRepo.Handshake
import encry.network.DeliveryManagerTests.DMUtils.{generateBlocks, initialiseDeliveryManager, toKey}
import encry.network.NodeViewSynchronizer.ReceivableMessages.{CheckDelivery, HandshakedPeer, OtherNodeSyncingStatus, RequestFromLocal}
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.ModifierId
import encry.view.history.EncryHistory
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class DeliveryManagerReRequestModifiesSpec extends WordSpecLike with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
  val settings: EncryAppSettings = EncryAppSettings.read

  override def afterAll(): Unit = system.terminate()

  "ReRequestModifies" should {
    val initialState = initialiseDeliveryManager(isBlockChainSynced = true, isMining = true, settings)
    val deliveryManager = initialState._1
    val newPeer = new InetSocketAddress("172.16.13.10", 9001)
    val peer: ConnectedPeer = ConnectedPeer(newPeer, deliveryManager, Incoming,
      Handshake(protocolToBytes(settings.network.appVersion), "peer", Some(newPeer), System.currentTimeMillis()))
    val blocks: (EncryHistory, List[Block]) = generateBlocks(10, generateDummyHistory(settings))
    val headerIds: List[ModifierId] = blocks._2.map(_.header.id)
    deliveryManager ! HandshakedPeer(peer)
    deliveryManager ! OtherNodeSyncingStatus(peer, Older, None)

    "re-ask necessary modifiers correctly" in {
      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, headerIds)
      headerIds.foreach { id =>
        deliveryManager ! CheckDelivery(peer, Header.modifierTypeId, id)
        assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
          .find(mId => mId._1 == toKey(id)).forall(elem => elem._2._2 == 2))
      }
    }
    "don't re-ask unnecessary modifiers" in {
      generateBlocks(10, generateDummyHistory(settings))._2.map(_.header.id)
        .foreach { id =>
          deliveryManager ! CheckDelivery(peer, Header.modifierTypeId, id)
          assert(deliveryManager.underlyingActor.expectedModifiers
            .getOrElse(peer.socketAddress.getAddress, Map.empty).isEmpty)
        }
    }
    "remove expired modifier form awaiting collection" in {
      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, headerIds)
      (0 until settings.network.maxDeliveryChecks).foreach(_ =>
        deliveryManager ! CheckDelivery(peer, Header.modifierTypeId, headerIds.head))
      assert(deliveryManager.underlyingActor.expectedModifiers
        .getOrElse(peer.socketAddress.getAddress, Map.empty).size == 9)
    }
  }
}