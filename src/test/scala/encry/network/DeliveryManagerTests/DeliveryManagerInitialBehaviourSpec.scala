package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import encry.network.DeliveryManagerTests.DMUtils._

class DeliveryManagerInitialBehaviourSpec extends WordSpecLike with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
  val settings: EncryAppSettings = EncryAppSettings.read

  override def afterAll(): Unit = system.terminate()

  "DeliveryManager" should {
    "handle only HistoryChange, HandshakedPeer, DisconnectedPeer " +
      "and don't handle other messages before history's initialisation" in {
      val history: EncryHistory = generateDummyHistory(settings)
      val deliveryManager: TestActorRef[DeliveryManager] =
        TestActorRef[DeliveryManager](DeliveryManager.props(None, TestProbe().ref, TestProbe().ref, settings, TestProbe().ref))

      val (_: InetSocketAddress, cp1: ConnectedPeer) = createPeer(9001, "172.16.13.10", settings)
      val (_: InetSocketAddress, cp2: ConnectedPeer) = createPeer(9001, "172.16.13.11", settings)

      deliveryManager ! HandshakedPeer(cp1)
      assert(deliveryManager.underlyingActor.syncTracker.statuses.contains(cp1.socketAddress.getAddress))
      assert(deliveryManager.underlyingActor.syncTracker.statuses.size == 1)

      deliveryManager ! HandshakedPeer(cp2)
      assert(deliveryManager.underlyingActor.syncTracker.statuses.contains(cp2.socketAddress.getAddress))
      assert(deliveryManager.underlyingActor.syncTracker.statuses.size == 2)

      deliveryManager ! DisconnectedPeer(cp1.socketAddress)
      assert(deliveryManager.underlyingActor.syncTracker.statuses.contains(cp2.socketAddress.getAddress))
      assert(deliveryManager.underlyingActor.syncTracker.statuses.size == 1)

      deliveryManager ! CheckModifiersToDownload

      deliveryManager ! UpdatedHistory(history)
    }
  }
}