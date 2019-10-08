package encry.view.fastSync

import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.fastSync.SnapshotHolder.{SnapshotChunk, SnapshotChunkSerializer, SnapshotManifest}
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.history.Block
import cats.syntax.option._
import encry.settings.EncryAppSettings
import encry.view.fastSync.SnapshotProcessor.{InfoToInsert, InfoToRemove}
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.utils.Algos

final case class SnapshotProcessor(actualManifest: Option[(SnapshotManifest, List[StorageKey])],
                                   potentialManifests: List[(SnapshotManifest, List[StorageKey])],
                                   bestPotentialManifest: Option[(SnapshotManifest, List[StorageKey])],
                                   settings: EncryAppSettings) {

  def processNewSnapshot(state: UtxoState, block: Block): (SnapshotProcessor, InfoToInsert) =
    potentialManifests find { case (manifest, _) =>
      (manifest.bestBlockId sameElements block.id) && (manifest.rootHash sameElements state.tree.rootHash)
    } match {
      case Some(elem) => updateBestPotentialSnapshot(elem)
      case None => createNewSnapshot(state, block)
    }

  def processNewBlock(block: Block): (SnapshotProcessor, InfoToRemove) =
    if (block.header.height > settings.snapshotSettings.creationHeight + settings.levelDB.maxVersions)
      updateActualSnapshot()
    else this -> InfoToRemove(List.empty[StorageKey])

  private def updateBestPotentialSnapshot(elem: (SnapshotManifest, List[StorageKey])): (SnapshotProcessor, InfoToInsert) =
    this.copy(bestPotentialManifest = elem.some) -> InfoToInsert(List.empty[(StorageKey, StorageValue)])

  private def createNewSnapshot(state: UtxoState, block: Block): (SnapshotProcessor, InfoToInsert) = {
    val (manifest: SnapshotManifest, chunks: List[SnapshotChunk]) = state.tree.initializeSnapshotData(block)
    val snapshotToDB: List[(StorageKey, StorageValue)] = chunks.map { elem =>
      val bytes: Array[Byte] = SnapshotChunkSerializer.toProto(elem).toByteArray
      StorageKey @@ Algos.hash(bytes) -> StorageValue @@ bytes
    }
    val infoForUpdate: (SnapshotManifest, List[StorageKey]) = manifest -> snapshotToDB.map(_._1)
    this.copy(
      potentialManifests = infoForUpdate :: potentialManifests,
      bestPotentialManifest = infoForUpdate.some
    ) -> InfoToInsert(snapshotToDB)
  }

  private def updateActualSnapshot(): (SnapshotProcessor, InfoToRemove) = {
    val newActualSnapshot: Option[(SnapshotManifest, List[StorageKey])] = bestPotentialManifest
    val manifestsToDelete: List[StorageKey] = potentialManifests.foldLeft(List.empty[StorageKey]) {
      case (toDelete, (manifest, _)) if newActualSnapshot.exists(_._1.ManifestId sameElements manifest.ManifestId) =>
        toDelete
      case (toDelete, (_, keys)) =>
        toDelete ::: keys
    }
    this.copy(
      actualManifest = newActualSnapshot,
      potentialManifests = List.empty,
      bestPotentialManifest = none
    ) -> InfoToRemove(manifestsToDelete)
  }
}

object SnapshotProcessor {

  def empty(settings: EncryAppSettings): SnapshotProcessor = new SnapshotProcessor(
    none[(SnapshotManifest, List[StorageKey])],
    List.empty[(SnapshotManifest, List[StorageKey])],
    none[(SnapshotManifest, List[StorageKey])],
    settings
  )

  final case class InfoToInsert(list: List[(StorageKey, StorageValue)]) extends AnyVal

  final case class InfoToRemove(list: List[StorageKey]) extends AnyVal

}