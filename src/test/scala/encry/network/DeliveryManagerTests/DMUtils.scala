package encry.network.DeliveryManagerTests

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.local.miner.Miner.StartMining
import encry.modifiers.InstanceFactory
import encry.modifiers.history.Block
import encry.network.DeliveryManager
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages.UpdatedHistory
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.ModifierId
import encry.view.history.EncryHistory
import scala.collection.mutable

object DMUtils extends InstanceFactory {

  def initialiseDeliveryManager(isBlockChainSynced: Boolean,
                                isMining: Boolean,
                                settings: EncryAppSettings)
                               (implicit actorSystem: ActorSystem): (TestActorRef[DeliveryManager], EncryHistory) = {
    val history: EncryHistory = generateDummyHistory(settings)
    val deliveryManager: TestActorRef[DeliveryManager] =
      TestActorRef[DeliveryManager](DeliveryManager.props(None, TestProbe().ref, TestProbe().ref, settings))
    deliveryManager ! UpdatedHistory(history)
    if (isMining) deliveryManager ! StartMining
    if (isBlockChainSynced) deliveryManager ! FullBlockChainIsSynced
    (deliveryManager, history)
  }

  def generateBlocks(qty: Int, history: EncryHistory): (EncryHistory, List[Block]) =
    (0 until qty).foldLeft(history, List.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block), blocks :+ block)
    }

  def toKey(id: ModifierId) = new mutable.WrappedArray.ofByte(id)

}
