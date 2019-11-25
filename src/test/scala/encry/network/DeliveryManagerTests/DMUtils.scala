package encry.network.DeliveryManagerTests

import java.io.File
import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages.UpdatedHistory
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.fast.sync.FastSyncTestsUtils.settings
import encry.view.history.History
import encry.view.state.avlTree.AvlTree
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.iq80.leveldb.Options

import scala.collection.mutable
import scala.collection.mutable.WrappedArray

object DMUtils extends InstanceFactory {

  def initialiseDeliveryManager(isBlockChainSynced: Boolean,
                                isMining: Boolean,
                                settings: EncryAppSettings)
                               (implicit actorSystem: ActorSystem): (TestActorRef[DeliveryManager], History) = {
    val history: History = generateDummyHistory(settings)
    val deliveryManager: TestActorRef[DeliveryManager] =
      TestActorRef[DeliveryManager](DeliveryManager
        .props(None, TestProbe().ref, TestProbe().ref, TestProbe().ref, TestProbe().ref, TestProbe().ref, settings))
    deliveryManager ! UpdatedHistory(history)
    if (isMining) deliveryManager ! StartMining
    else deliveryManager ! DisableMining
    if (isBlockChainSynced) deliveryManager ! FullBlockChainIsSynced
    (deliveryManager, history)
  }

  def generateBlocks(qty: Int, history: History): (History, List[Block]) =
    (0 until qty).foldLeft(history, List.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header)
        prevHistory.append(block.payload)
        val a = prevHistory.reportModifierIsValid(block)
        (a, blocks :+ block)
    }

  def generateBlocksWithTree(qty: Int,
                             history: History,
                             prevTreeOpt: Option[AvlTree[StorageKey, StorageValue]] = None,
                             addressOpt: Option[Address] = None): (History, List[Block], AvlTree[StorageKey, StorageValue]) = {

    import encry.view.state.avlTree.utils.implicits.Instances._

    val avl: AvlTree[StorageKey, StorageValue] = prevTreeOpt match {
      case Some(t) => t
      case None =>
        val dir: File = FileHelper.getRandomTempDir
        val storage: VLDBWrapper = {
          val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
          VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
        }

        AvlTree[StorageKey, StorageValue](storage)
    }

    (0 until qty).foldLeft(history, List.empty[Block], avl) {
      case ((prevHistory, blocks, tree), _) =>
        val (block: Block, newTree: AvlTree[StorageKey, StorageValue]) =
          generateNextBlockAndInsert(prevHistory, tree, prevTreeOpt.isEmpty, addressOpt = addressOpt)
        prevHistory.append(block.header)
        prevHistory.append(block.payload)
        val newHist = prevHistory.reportModifierIsValid(block)
        (newHist, blocks :+ block, newTree)
    }
  }

  def toKey(id: ModifierId): WrappedArray.ofByte = new mutable.WrappedArray.ofByte(id)

  def createPeer(port: Int,
                 host: String,
                 settings: EncryAppSettings)(implicit system: ActorSystem): (InetSocketAddress, ConnectedPeer) = {
    val address = new InetSocketAddress(host, port)
    val peer: ConnectedPeer = ConnectedPeer(address, TestProbe().ref, Incoming,
      Handshake(protocolToBytes(settings.network.appVersion), host, Some(address), System.currentTimeMillis()))
    (address, peer)
  }
}