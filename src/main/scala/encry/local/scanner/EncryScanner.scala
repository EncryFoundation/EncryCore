package encry.local.scanner

import java.io.File

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import encry.local.scanner.storage.{EncryIndexReader, IndexStorage}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.{EncryBaseBox, EncryBox}
import encry.settings.{Algos, Constants, EncryAppSettings}
import encry.storage.codec.FixLenComplexValueCodec
import io.circe.Json
import io.circe.syntax._
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.NodeViewHolder.{SemanticallySuccessfulModifier, Subscribe}
import scorex.core.utils.ScorexLogging
import scorex.core.{NodeViewHolder, VersionTag}
import scorex.crypto.authds.ADKey

import scala.collection.mutable

class EncryScanner(settings: EncryAppSettings,
                   viewHolderRef: ActorRef,
                   indexStore: Store) extends Actor with ScorexLogging {

  import EncryScanner._
  import IndexStorage._

  protected lazy val storage: IndexStorage = new IndexStorage(indexStore)

  protected lazy val indexReader: EncryIndexReader = new EncryIndexReader(storage)

  def version: VersionTag = storage.get(IndexStorage.IndexVersionKey)
    .map(VersionTag @@ _).getOrElse(InitialVersion)

  def lastScannedHeaderOpt: Option[EncryBlockHeader] = storage.get(IndexStorage.LastScannedBlockKey)
    .flatMap(r => EncryBlockHeaderSerializer.parseBytes(r).toOption)

  override def preStart(): Unit = {
    val events = Seq(
      NodeViewHolder.EventType.StateChanged,
      NodeViewHolder.EventType.SuccessfulSemanticallyValidModifier
    )
    viewHolderRef ! Subscribe(events)
  }

  override def receive: Receive = {

    case SemanticallySuccessfulModifier(mod: EncryPersistentModifier) =>
      scanPersistent(mod)

    case GetIndexReader =>
      sender ! IndexReader(indexReader)

    case GetScannerStatus =>
      sender ! ScannerStatus(version, lastScannedHeaderOpt)
  }

  private def scanPersistent(mod: EncryPersistentModifier): Unit = mod match {
      case block: EncryBlock =>
        updateIndex(IndexMetadata(VersionTag @@ block.id, block.header), scanTransactions(block.payload.transactions))
      case _ => // Do nothing.
    }

  private def scanTransactions(txs: Seq[EncryBaseTransaction]): ScanningResult = {
    val boxIdsToRemove = txs.flatMap(_.unlockers.map(_.boxId))
    val (newIndexes, boxesToInsert) = txs.flatMap(_.newBoxes)
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
    ScanningResult(newIndexes.toSeq, boxesToInsert, boxIdsToRemove)
  }

  private def updateIndex(md: IndexMetadata, sr: ScanningResult): Unit = {
    val currentIndexes = sr.newIndexes ++ sr.toRemove.foldLeft(Seq[(ByteArrayWrapper, Seq[ADKey])]())((acc, id) =>
      storage.get(keyByBoxId(id)).map(r => acc :+ ByteArrayWrapper(r) -> Seq.empty).getOrElse(acc))
    val finalIndexes = currentIndexes.foldLeft(Seq[(ByteArrayWrapper, Seq[ADKey])]()) { case (acc, (pk, ids))  =>
      storage.get(pk).map(r => acc :+ pk -> FixLenComplexValueCodec.parseComplexValue(r, EncryBox.BoxIdSize)
        .map(bxIds => bxIds.filter(id => !sr.toRemove.exists(_.sameElements(id))).map(ADKey @@ _) ++ ids)
        .getOrElse(Seq.empty)).getOrElse(acc :+ pk -> ids)
    }
    val toInsert = finalIndexes.map { case (pk, ids) =>
      pk -> FixLenComplexValueCodec.toComplexValue(ids)
    } ++ sr.toInsert.map(bx => keyByBoxId(bx.id) -> ByteArrayWrapper(Algos.hash(bx.proposition.bytes)))
    val toRemove = sr.toRemove.map(ByteArrayWrapper.apply)

    storage.update(ByteArrayWrapper(md.version), toRemove, toInsert)
  }
}

object EncryScanner {

  case object GetIndexReader

  case class IndexReader(reader: EncryIndexReader)

  case class ScanningResult(newIndexes: Seq[(ByteArrayWrapper, Seq[ADKey])],
                            toInsert: Seq[EncryBaseBox],
                            toRemove: Seq[ADKey])

  case class IndexMetadata(version: VersionTag, header: EncryBlockHeader)

  case object GetScannerStatus

  case class ScannerStatus(version: VersionTag, lastScannedHeader: Option[EncryBlockHeader]) {
    lazy val json: Json = Map(
      "version" -> Algos.encode(version).asJson,
      "lastScannedHeader" -> lastScannedHeader.map(_.json).getOrElse("None".asJson)
    ).asJson
  }

  val InitialVersion: VersionTag = VersionTag @@ Algos.hash("initial_version")

  def getIndexDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/index")

  def readOrCreate(settings: EncryAppSettings, viewHolderRef: ActorRef): EncryScanner = {

    val indexDir = getIndexDir(settings)
    indexDir.mkdirs()

    val indexStore = new LSMStore(indexDir, keepVersions = Constants.keepVersions)

    new EncryScanner(settings, viewHolderRef, indexStore)
  }
}

object EncryScannerRef {

  def props(settings: EncryAppSettings,
            viewHolderRef: ActorRef): Props =
    Props(EncryScanner.readOrCreate(settings, viewHolderRef))

  def apply(settings: EncryAppSettings,
            viewHolderRef: ActorRef)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(settings, viewHolderRef))
}
