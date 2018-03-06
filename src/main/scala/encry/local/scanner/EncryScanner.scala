package encry.local.scanner

import akka.actor.{Actor, ActorRef}
import encry.local.scanner.storage.IndexStorage
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.{EncryBaseBox, EncryBox}
import encry.settings.EncryAppSettings
import encry.storage.codec.FixLenComplexValueCodec
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.{NodeViewHolder, VersionTag}
import scorex.core.NodeViewHolder.Subscribe
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey

import scala.collection.mutable

case class ScanningResult(indexChunk: Seq[(ByteArrayWrapper, Seq[ADKey])],
                          toInsert: Seq[EncryBaseBox],
                          toRemove: Seq[ADKey])

class EncryScanner(val version: VersionTag,
                   settings: EncryAppSettings,
                   viewHolderRef: ActorRef,
                   indexStore: Store) extends Actor with ScorexLogging {

  import IndexStorage._

  protected lazy val storage: IndexStorage = new IndexStorage(indexStore)

  override def preStart(): Unit = {
    val events = Seq(NodeViewHolder.EventType.StateChanged)
    viewHolderRef ! Subscribe(events)
  }

  override def receive: Receive = ???

  private def scanPersistent(mod: EncryPersistentModifier): EncryScanner = mod match {
      case block: EncryBlock =>
        val version = VersionTag @@ block.id
        updateIndex(version, scanTransactions(block.payload.transactions))
        new EncryScanner(VersionTag @@ block.id, settings, viewHolderRef, indexStore)
      case payload: EncryBlockPayload =>
        val version = VersionTag @@ payload.headerId
        updateIndex(version, scanTransactions(payload.transactions))
        new EncryScanner(VersionTag @@ payload.headerId, settings, viewHolderRef, indexStore)
    }

  private def scanTransactions(txs: Seq[EncryBaseTransaction]): ScanningResult = {
    val boxIdsToRemove = txs.flatMap(_.unlockers.map(_.boxId))
    val (indexChunk, boxesToInsert) = txs.flatMap(_.newBoxes)
      .foldLeft(mutable.TreeMap[ByteArrayWrapper, Seq[ADKey]](), Seq[EncryBaseBox]()) { case ((cache, bxs), bx) =>
        if (!boxIdsToRemove.exists(_.sameElements(bx.id))) {
          cache.get(keyByProposition(bx.proposition)) match {
            case Some(ids) => cache.update(keyByProposition(bx.proposition), ids :+ bx.id)
            case _ => cache.update(keyByProposition(bx.proposition), Seq(bx.id))
          }
          cache -> (bxs :+ bx)
        } else {
          cache -> bxs
        }
      }
    ScanningResult(indexChunk.toSeq, boxesToInsert, boxIdsToRemove)
  }

  private def updateIndex(version: VersionTag, sr: ScanningResult): Unit = {
    val currentIndexes = sr.indexChunk ++ sr.toRemove.foldLeft(Seq[(ByteArrayWrapper, Seq[ADKey])]())((acc, id) =>
      storage.get(keyByBoxId(id)).map(r => acc :+ ByteArrayWrapper(r) -> Seq.empty).getOrElse(acc))
    val indexChunkFinal = currentIndexes.foldLeft(Seq[(ByteArrayWrapper, Seq[ADKey])]()) { case (acc, (pk, ids))  =>
      storage.get(pk).map(r => FixLenComplexValueCodec.parseComplexValue(r, EncryBox.BoxIdSize)
        .map(bxIds => bxIds.filter(id => !sr.toRemove.exists(_.sameElements(id))) ++ ids)).flatten.getOrElse(acc)
    }
    val toInsert = indexChunkFinal.map { case (pk, ids) =>
      pk -> FixLenComplexValueCodec.toComplexValue(ids)
    } ++ sr.toInsert.map(bx => keyByBoxId(bx.id) -> ByteArrayWrapper(bx.bytes))
    val toRemove = sr.toRemove.map(ByteArrayWrapper.apply)

    storage.update(ByteArrayWrapper(version), toRemove, toInsert)
  }
}
