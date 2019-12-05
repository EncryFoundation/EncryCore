package encry.view.nvh

import akka.actor.ActorSystem
import akka.testkit.{TestActor, TestActorRef, TestProbe}
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider

object NVHUtils {

//  def initNvh(settings: EncryAppSettings)(implicit actorSystem: ActorSystem): TestActorRef[EncryNodeViewHolder[UtxoState]] = {
//    val fakeActorRef = TestProbe()
//    val ntp: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)
//    TestActorRef[EncryNodeViewHolder[UtxoState]](EncryNodeViewHolder.props(
//      settings,
//      ntp,
//      fakeActorRef.ref,
//      None,
//      fakeActorRef.ref,
//      fakeActorRef.ref
//    ))
//  }
}
