//package encry.view.fast.sync
//
//import java.net.InetSocketAddress
//
//import akka.actor.ActorSystem
//import akka.testkit.TestProbe
//import encry.modifiers.InstanceFactory
//import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
//import encry.settings.{EncryAppSettings, TestNetSettings}
//import encry.utils.FileHelper
//import encry.view.fast.sync.SnapshotHolder.SnapshotManifest.{ChunkId, ManifestId}
//import encry.view.fast.sync.SnapshotHolder.{SnapshotManifest, SnapshotManifestSerializer}
//import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
//import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}
//import scorex.utils.Random
//
//class SnapshotDownloadControllerTest
//    extends WordSpecLike
//    with Matchers
//    with InstanceFactory
//    with OneInstancePerTest
//    with TestNetSettings {
//  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
//
//  "Snapshot download controller" should {
//    "process new manifest message correctly" in {
//      val settingsWithRandomDir = EncryAppSettings
//        .read()
//        .copy(
//          directory = FileHelper.getRandomTempDir.toString
//        )
//      val snapshotDownloadController = SnapshotDownloadController.empty(settingsWithRandomDir)
//      val history                    = generateDummyHistory(settings)
//      val randomChunks               = (0 to 20001).map(_ => ChunkId @@ Random.randomBytes()).toList
//      val randomManifest = SnapshotManifestSerializer.toProto(
//        SnapshotManifest(
//          ManifestId @@ Random.randomBytes(),
//          randomChunks
//        )
//      )
//      val address = new InetSocketAddress("0.0.0.0", 9000)
//      val peer: ConnectedPeer = ConnectedPeer(
//        address,
//        TestProbe().ref,
//        Incoming,
//        Handshake(protocolToBytes(settings.network.appVersion), "0.0.0.0", Some(address), System.currentTimeMillis())
//      )
//      val newController = snapshotDownloadController.processManifest(
//        randomManifest,
//        peer,
//        history
//      )
//
//      val requiredBatchesSize =
//        (randomChunks.size / settingsWithRandomDir.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod) + 1
//
//      newController.isRight shouldBe true
//      newController.right.get.batchesSize shouldBe requiredBatchesSize
//    }
//    "provide correct getNextBatchAndRemoveItFromController function" in {
//      val settingsWithRandomDir = EncryAppSettings
//        .read()
//        .copy(
//          directory = FileHelper.getRandomTempDir.toString
//        )
//      val snapshotDownloadController = SnapshotDownloadController.empty(settingsWithRandomDir)
//      val history                    = generateDummyHistory(settings)
//      val randomChunks               = (1 to 20000).map(_ => ChunkId @@ Random.randomBytes()).toList
//      val randomManifest = SnapshotManifestSerializer.toProto(
//        SnapshotManifest(
//          ManifestId @@ Random.randomBytes(),
//          randomChunks
//        )
//      )
//      val address = new InetSocketAddress("0.0.0.0", 9000)
//      val peer: ConnectedPeer = ConnectedPeer(
//        address,
//        TestProbe().ref,
//        Incoming,
//        Handshake(protocolToBytes(settings.network.appVersion), "0.0.0.0", Some(address), System.currentTimeMillis())
//      )
//      val newController = snapshotDownloadController.processManifest(
//        randomManifest,
//        peer,
//        history
//      )
//      val requiredBatchesSize = randomChunks.size / settingsWithRandomDir.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod
//
//      val nextController = newController.right.get.getNextBatchAndRemoveItFromController
//      nextController.isRight shouldBe true
//      nextController.right.get._1.nextGroupForRequestNumber shouldBe 1
//      nextController.right.get._1.batchesSize shouldBe requiredBatchesSize - 1
//
//      val nextController1 = (nextController.right.get._1.nextGroupForRequestNumber until requiredBatchesSize)
//        .foldLeft(nextController.right.get._1) {
//          case (controllerN, _) =>
//            controllerN.getNextBatchAndRemoveItFromController.right.get._1
//        }
//
//      nextController1.batchesSize shouldBe 0
//
//      nextController1.getNextBatchAndRemoveItFromController.isLeft shouldBe true
//
//    }
//  }
//}
