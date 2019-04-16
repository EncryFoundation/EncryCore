package encry.network

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestActorRef
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider

object PeerManagerUtils {

  def initPeerManager(nvsRef: ActorRef,
                      settings: EncryAppSettings,
                      ntp: NetworkTimeProvider)(implicit actorSystem: ActorSystem): TestActorRef[PeerManager] =
    TestActorRef[PeerManager](Props(new PeerManager(nvsRef, settings, ntp)))
}
