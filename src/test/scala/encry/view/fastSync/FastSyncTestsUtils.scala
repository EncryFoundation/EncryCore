package encry.view.fastSync

import java.io.File
import java.net.InetSocketAddress

import akka.testkit.TestProbe
import encry.modifiers.InstanceFactory
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.state.avlTree.AvlTree
import org.iq80.leveldb.Options
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake

object FastSyncTestsUtils extends InstanceFactory with TestNetSettings {

  def createAvl(address: String, from: Int, to: Int): AvlTree[StorageKey, StorageValue] = {
    val firstDir: File = FileHelper.getRandomTempDir
    val firstStorage: VLDBWrapper = {
      val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }
    val boxes: IndexedSeq[(StorageKey, StorageValue)] = (from to to)
      .map(i => genAssetBox(address, i, nonce = i))
      .map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

    val firstAvl: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](firstStorage)
    firstAvl
      .insertAndDeleteMany(
        StorageVersion @@ Random.randomBytes(),
        boxes.toList,
        List.empty
      )
  }

  def tmpDir: File = FileHelper.getRandomTempDir
}
