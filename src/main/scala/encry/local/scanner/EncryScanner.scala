package encry.local.scanner

import java.io.File

import akka.actor.{Actor, Props}
import encry.local.scanner.storage.{EncryIndexReader, IndexStorage}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.EncryBaseBox
import encry.settings.{Algos, Constants, EncryAppSettings}
import encry.storage.codec.FixLenComplexValueCodec
import encry.EncryApp._
import io.circe.Json
import io.circe.syntax._
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import encry.VersionTag
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.{ChangedState, RollbackSucceed, SemanticallySuccessfulModifier}
import encry.utils.ScorexLogging
import scorex.crypto.authds.ADKey

import scala.collection.mutable

class EncryScanner(indexStore: Store) extends Actor with ScorexLogging {

  import EncryScanner._
  import IndexStorage._

  val storage: IndexStorage = IndexStorage(indexStore)

  val indexReader: EncryIndexReader = EncryIndexReader(storage)

  def version: VersionTag = storage.get(IndexStorage.IndexVersionKey).map(VersionTag @@ _)
    .getOrElse(VersionTag @@ Algos.hash("initial_version"))

  def lastScannedHeaderOpt: Option[EncryBlockHeader] = storage.get(IndexStorage.LastScannedBlockKey)
    .flatMap(r => EncryBlockHeaderSerializer.parseBytes(r).toOption)

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[RollbackSucceed])
    context.system.eventStream.subscribe(self, classOf[ChangedState[_]])
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {

    case SemanticallySuccessfulModifier(mod: EncryPersistentModifier) => scanPersistent(mod)
    case RollbackSucceed(branchPointOpt) => branchPointOpt.foreach(storage.rollbackTo)
    case GetIndexReader => sender ! IndexReader(indexReader)
    case GetScannerStatus => sender ! ScannerStatus(version, lastScannedHeaderOpt)
  }

  def scanPersistent(mod: EncryPersistentModifier): Unit = mod match {
    case block: EncryBlock =>
      updateIndex(IndexMetadata(VersionTag @@ block.id, block.header), scanTransactions(block.payload.transactions))
    case _ =>
  }

  def scanTransactions(txs: Seq[EncryBaseTransaction]): ScanningResult = {
    val boxIdsToRemove: Seq[ADKey] = txs.flatMap(_.inputs.map(_.boxId))
    val (newIndexes, boxesToInsert) = txs.flatMap(_.newBoxes)
      .foldLeft(mutable.TreeMap[ByteArrayWrapper, Seq[ADKey]](), Seq[EncryBaseBox]()) { case ((cache, bxs), bx) =>
        if (!boxIdsToRemove.exists(_.sameElements(bx.id))) {
          cache.get(keyByProposition(bx.proposition)) match {
            case Some(ids) => cache.update(keyByProposition(bx.proposition), ids :+ bx.id)
            case None => cache.update(keyByProposition(bx.proposition), Seq(bx.id))
          }
          cache -> (bxs :+ bx)
        } else cache -> bxs
      }
    ScanningResult(newIndexes.toSeq, boxesToInsert, boxIdsToRemove)
  }

  def updateIndex(md: IndexMetadata, sr: ScanningResult): Unit = {
    val toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = sr.newIndexes
      .foldLeft(Seq[(ByteArrayWrapper, ByteArrayWrapper)]()) { case (acc, (key, ids)) =>
        acc :+ (key -> FixLenComplexValueCodec.toComplexValue(
          ids.foldLeft(Seq[ADKey]()) { case (a, k) =>
            if (!sr.toRemove.contains(k)) a :+ k else a
          } ++ Seq(storage.get(key).getOrElse(Array.emptyByteArray))))
      } ++ sr.toInsert.foldLeft(Seq[(ByteArrayWrapper, ByteArrayWrapper)]()) { case (seq, bx) =>
      if (!sr.toRemove.contains(bx.id)) seq :+ (ByteArrayWrapper(bx.id) -> ByteArrayWrapper(bx.bytes))
      else seq
    }
    val toRemove: Seq[ByteArrayWrapper] = sr.toRemove.map(ByteArrayWrapper.apply) ++ sr.newIndexes.map(_._1)
    storage.update(ByteArrayWrapper(md.version), toRemove, toInsert)
  }
}

object EncryScanner {

  case object GetIndexReader

  case object GetScannerStatus

  case class IndexReader(reader: EncryIndexReader)

  case class ScanningResult(newIndexes: Seq[(ByteArrayWrapper, Seq[ADKey])],
                            toInsert: Seq[EncryBaseBox],
                            toRemove: Seq[ADKey])

  case class IndexMetadata(version: VersionTag, header: EncryBlockHeader)

  case class ScannerStatus(version: VersionTag, lastScannedHeader: Option[EncryBlockHeader]) {
    lazy val json: Json = Map(
      "version" -> Algos.encode(version).asJson,
      "lastScannedHeader" -> lastScannedHeader.map(_.asJson).getOrElse("None".asJson)
    ).asJson
  }

  def getIndexDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/index")

  def props(): Props = {
    val indexDir: File = getIndexDir(settings)
    indexDir.mkdirs()
    val indexStore: LSMStore = new LSMStore(indexDir, keepVersions = Constants.DefaultKeepVersions)
    Props(classOf[EncryScanner], indexStore)
  }
}